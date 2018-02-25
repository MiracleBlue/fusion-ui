module Fusion.DOM.Props (
  module Exports
) where

import Prelude

import Data.Generic.Rep (class Generic)
import Fusion.Utils (class Showable, toString, asWords, asPascalCase)
import React (Event, handle)
import React.DOM.Props as Exports

-- Everything beneath is purely experimental still, so please don't use

data DOMEvent = Click
              | Submit
              | Load
              | Copy
              | Cut
              | Paste
              | KeyDown
              | KeyPress
              | KeyUp
              | Focus
              | Blur
              | Change
              | Input
              | Invalid
              | DoubleClick
              | Drag
              | DragEnd
              | DragEnter
              | DragExit
              | DragLeave
              | DragOver
              | DragStart
              | Drop
              | MouseDown
              | MouseEnter
              | MouseLeave
              | MouseMove
              | MouseOut
              | MouseOver
              | MouseUp
              | TouchCancel
              | TouchEnd
              | TouchMove
              | TouchStart
              | Scroll
              | Wheel
              | AnimationStart
              | AnimationEnd
              | AnimationIteration
              | TransitionEnd

-- derive instance genericDOMEvent :: Generic DOMEvent _
--
-- type EventAction eff = Event -> Eff eff Unit
--
-- getEventString :: Showable a => a -> String
-- getEventString ev = "on" <> asPascalCase ev
--
-- on :: forall eff. DOMEvent -> EventAction eff -> Props
-- on ev action = unsafeMkProps (getEventString ev) (handle action)
