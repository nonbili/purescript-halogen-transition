# purescript-halogen-transition

Currently only works with Halogen 5. I don't mind maintaining a Halogen 4 branch if there is any interest.

See a [demo](https://nonbili.github.io/purescript-halogen-transition/#Simple).

## How to use

Please check examples folder. It's as simple as

```
import Halogen.Transition as Transition

render state =
    HH.slot _transition unit Transition.component
    { enterClass: "simple-enter"
    , enterActiveClass: "simple-enter-active"
    , enterTimeout: Milliseconds 1000.0
    , leaveClass: "simple-leave"
    , leaveActiveClass: "simple-leave-active"
    , leaveTimeout: Milliseconds 600.0
    , shown: state.shown
    , render: HH.text "hello world!"
    } absurd
```
