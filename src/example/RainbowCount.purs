module Fusion.Example.RainbowCount where

import Prelude hiding (div)

import Fusion
import Fusion.DOM (a, button, div, h2, li, span, ul, blank, text, (!))
import Fusion.DOM.Props (className, href, onClick)

data CountEvents = Increment | Decrement

countingThing :: forall tab state opts.
                 Show           tab =>
                 Options        { counter :: Int } opts =>
                 HTML:Component { name :: String, tab :: tab | opts }
countingThing =
  withDefaults { counter: 0 } >>> render where
    handler :: Handle CountEvents _
    handler Increment state@{ counter } = state { counter = counter + 1 }
    handler Decrement state@{ counter } = state { counter = counter - 1 }
    render { name, tab, counter } children =
      withState { counter } handler view where
        view { counter } dispatch = div do
          children
          text $ "hello there: " <> show counter
          button ! onClick (const Increment >>> dispatch) $ do text "Increment"
          button ! onClick (const Decrement >>> dispatch) $ do text "Decrement"
