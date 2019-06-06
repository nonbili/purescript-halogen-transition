module Example.Simple where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Transition as Transition

type Query = Const Void

data Action
  = OnClick
  | HandleTransition (Transition.Message Action)

type State =
  { shown :: Boolean
  }

type Slots = (transition :: Transition.Slot Action Unit)

_transition = SProxy :: SProxy "transition"

type HTML = H.ComponentHTML Action Slots Aff

type DSL = H.HalogenM State Action Slots Void Aff

initialState :: State
initialState =
  { shown: true
  }

renderInner :: Transition.HTML Action Aff
renderInner =
  HH.div
  [ HE.onClick $ Just <<< const (Transition.raise OnClick) ]
  [ HH.text "hello world!" ]

render :: State -> HTML
render state =
  HH.div_
  [ HH.button
    [ HE.onClick $ Just <<< const OnClick ]
    [ HH.text "toggle" ]
  , HH.slot _transition unit Transition.component
    { enterClass: "simple-enter"
    , enterActiveClass: "simple-enter-active"
    , leaveClass: "simple-leave"
    , leaveActiveClass: "simple-leave-active"
    , shown: state.shown
    , render: renderInner
    } $ Just <<< HandleTransition
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
  }

handleAction :: Action -> DSL Unit
handleAction = case _ of
  OnClick -> do
    H.modify_ $ \s -> s { shown = not s.shown }

  HandleTransition msg -> do
    handleAction msg
