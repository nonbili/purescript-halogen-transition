module Example.Simple where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff, Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Transition as Transition

data Query a
  = OnClick a

type State =
  { shown :: Boolean
  }

type Slots = (transition :: Transition.Slot Unit Void Aff)

_transition = SProxy :: SProxy "transition"

type HTML = H.ComponentHTML Query Slots Aff

type DSL = H.HalogenM State Query Slots Void Aff

initialState :: State
initialState =
  { shown: true
  }

render :: State -> HTML
render state =
  HH.div_
  [ HH.button
    [ HE.onClick $ HE.input_ OnClick ]
    [ HH.text "toggle" ]
  , HH.slot _transition unit Transition.component
    { enterClass: "simple-enter"
    , enterActiveClass: "simple-enter-active"
    , enterTimeout: Milliseconds 500.0
    , leaveClass: "simple-leave"
    , leaveActiveClass: "simple-leave-active"
    , leaveTimeout: Milliseconds 300.0
    , shown: state.shown
    , render: HH.text "hello world!"
    } absurd
  ]

component :: H.Component HH.HTML Query Unit Void Aff
component = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> DSL
  eval (OnClick n) = n <$ do
    H.modify_ $ \s -> s { shown = not s.shown }
