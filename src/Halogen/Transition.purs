module Halogen.Transition
  ( Message
  , HTML
  , Action
  , raise
  , component
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
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
  , leaveClass :: String
  , leaveActiveClass :: String
  , shown :: Boolean
  , render :: HTML f m
  }

type Message f = f Unit

data Action f
  = HandleTransitionEnd
  | Raise (f Unit)

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

type HTML f m = H.ComponentHTML' (Action f) () m

raise :: forall f. f Unit -> Action f
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
  [ HP.class_ $ H.ClassName cls
  , HE.onTransitionEnd $ const $ Just HandleTransitionEnd
  ] $ join
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
  => H.Component HH.HTML (Const Void) (Props f m) (Message f) m
component = H.mkComponent
  { initialState
  , render
  , eval
  }
  where
  handleProps = do
    state <- H.get
    if state.props.shown
      then do
        H.modify_ $ _ { shown = true, transitionState = Enter }
        H.liftAff $ Aff.delay $ Milliseconds 1.0
        H.modify_ $ _ { transitionState = EnterActive }
      else
        if state.shown && not state.props.shown
          then do
            H.modify_ $ _ { transitionState = Leave }
            H.liftAff $ Aff.delay $ Milliseconds 1.0
            H.modify_ $ _ { transitionState = LeaveActive }
          else pure unit

  eval
    :: H.HalogenQ (Const Void) (Action f) (Props f m)
    ~> H.HalogenM' (State f m) (Action f) () (Message f) m
  eval = case _ of
    H.Initialize n -> n <$ handleProps

    H.Finalize n -> pure n

    H.Receive props n -> n <$ do
      state <- H.get
      H.modify_ $ _ { props = props }
      when (state.props.shown /= props.shown)
        handleProps

    H.Handle act n -> n <$ case act of
      HandleTransitionEnd -> do
        H.modify_ $ \s ->
          if s.transitionState == LeaveActive
            then s { shown = false, transitionState = Done }
            else s { transitionState = Done }
      Raise f -> H.raise f

    H.Request fa -> absurd (un Const fa)
