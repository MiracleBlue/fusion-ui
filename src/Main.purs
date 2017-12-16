module Main where

import Prelude

import Example.PowerLevel (powerLevelDemo)
import Fusion.Utils (RenderedEffect, renderToDom)

main :: forall eff. RenderedEffect eff
main = renderToDom "app" powerLevelDemo
