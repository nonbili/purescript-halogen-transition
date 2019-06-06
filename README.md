# purescript-halogen-transition

[![CircleCI](https://circleci.com/gh/nonbili/purescript-halogen-transition.svg?style=svg)](https://circleci.com/gh/nonbili/purescript-halogen-transition)

Works with Halogen 5 rc.4.

See a [demo](https://nonbili.github.io/purescript-halogen-transition/#Simple).

## How to use

Please check examples folder. The basic idea is passing a render function to `Transition.component`.

```
import Halogen.Transition as Transition

render state =
    HH.slot _transition unit Transition.component
    { enterClass: "simple-enter"
    , enterActiveClass: "simple-enter-active"
    , leaveClass: "simple-leave"
    , leaveActiveClass: "simple-leave-active"
    , shown: state.shown
    , render: HH.text "hello world!"
    } $ const Nothing
```
