module Halogen.Transition where

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

type Props i o m =
  { enterClass :: String
  , enterActiveClass :: String
  , enterTimeout :: Milliseconds
  , leaveClass :: String
  , leaveActiveClass :: String
  , leaveTimeout :: Milliseconds
  , shown :: Boolean
  , render :: HTML i o m
  }

data Query i o m a
  = OnEnter a
  | HandleChild o a
  | ReceiveProps (Props i o m) a

data TransitionState
  = Enter
  | EnterActive
  | Leave
  | LeaveActive
  | Done

derive instance eqTransitionState :: Eq TransitionState

type State i o m =
  { props :: Props i o m
  , shown :: Boolean
  , transitionState :: TransitionState
  }

type HTML i o m = H.ComponentHTML (Query i o m) () m

type DSL i o m = H.HalogenM (State i o m) (Query i o m) () o m

type Slot i o m = H.Slot (Query i o m) o Unit

initialState :: forall i o m. Props i o m -> State i o m
initialState props =
  { props
  , shown: props.shown
  , transitionState: Done
  }

render :: forall i o m. State i o m -> HTML i o m
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
  :: forall i o m
   . MonadAff m
  => H.Component HH.HTML (Query i o m) (Props i o m) o m
component = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input ReceiveProps
  , initializer: Just $ H.action OnEnter
  , finalizer: Nothing
  }
  where
  handleProps :: DSL i o m Unit
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

  eval :: Query i o m ~> DSL i o m
  eval (OnEnter n) = n <$ do
    handleProps

  eval (HandleChild o n) = n <$ do
    H.raise o

  eval (ReceiveProps props n) = n <$ do
    H.modify_ $ _ { props = props }
    handleProps
