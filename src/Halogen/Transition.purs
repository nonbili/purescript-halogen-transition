module Halogen.Transition
  ( Query
  , Message
  , Slot
  , HTML
  , raise
  , component
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Props f m =
  { enterClass :: String
  , enterActiveClass :: String
  , enterTimeout :: Milliseconds
  , leaveClass :: String
  , leaveActiveClass :: String
  , leaveTimeout :: Milliseconds
  , shown :: Boolean
  , render :: HTML f m
  }

type Message f = f Unit

data Query f m a
  = OnEnter a
  | ReceiveProps (Props f m) a
  | Raise (f Unit) a

data TransitionState
  = Enter
  | EnterActive
  | Leave
  | LeaveActive
  | Done

derive instance eqTransitionState :: Eq TransitionState

type State f m =
  { props :: Props f m
  , shown :: Boolean
  , transitionState :: TransitionState
  }

type HTML f m = H.ComponentHTML (Query f m) () m

type DSL f m = H.HalogenM (State f m) (Query f m) () (Message f) m

type Slot f m s = H.Slot (Query f m) (Message f) s

raise :: forall f m a. f Unit -> a -> Query f m a
raise f = Raise f

initialState :: forall f m. Props f m -> State f m
initialState props =
  { props
  , shown: props.shown
  , transitionState: Done
  }

render :: forall f m. State f m -> HTML f m
render { shown, props, transitionState } =
  HH.div
  [ HP.class_ $ H.ClassName cls ] $ join
  [ guard shown $> props.render
  ]
  where
  cls = case transitionState of
    Enter -> props.enterClass
    EnterActive -> props.enterActiveClass
    Leave -> props.leaveClass
    LeaveActive -> props.leaveActiveClass
    _ -> ""

component
  :: forall f m
   . MonadAff m
  => H.Component HH.HTML (Query f m) (Props f m) (Message f) m
component = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input ReceiveProps
  , initializer: Just $ H.action OnEnter
  , finalizer: Nothing
  }
  where
  handleProps :: DSL f m Unit
  handleProps = do
    state <- H.get
    if state.props.shown
      then do
        H.modify_ $ _ { shown = true, transitionState = Enter }
        H.liftAff $ Aff.delay $ Milliseconds 1.0
        H.modify_ $ _ { transitionState = EnterActive }
        H.liftAff $ Aff.delay state.props.enterTimeout
        H.modify_ $ \s ->
          if s.transitionState == EnterActive
            then s { transitionState = Done }
            else s
      else
        if state.shown && not state.props.shown
          then do
            H.modify_ $ _ { transitionState = Leave }
            H.liftAff $ Aff.delay $ Milliseconds 1.0
            H.modify_ $ _ { transitionState = LeaveActive }
            H.liftAff $ Aff.delay state.props.leaveTimeout
            H.modify_ $ \s ->
              if s.transitionState == LeaveActive
                then s { shown = false, transitionState = Done }
                else s
          else pure unit

  eval :: Query f m ~> DSL f m
  eval (OnEnter n) = n <$ do
    handleProps

  eval (ReceiveProps props n) = n <$ do
    state <- H.get
    H.modify_ $ _ { props = props }
    when (state.shown /= props.shown) $ do
      handleProps

  eval (Raise pq n) = n <$ do
    H.raise pq
