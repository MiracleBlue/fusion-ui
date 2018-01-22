module Main where

import Prelude

import Fusion.Example.PowerLevel (powerLevelDemo)
import Fusion.Utils (renderToDom)

-- main :: forall eff. RenderedEffect eff
main = renderToDom "app" powerLevelDemo
