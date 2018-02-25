module Example.Main where

import Fusion.Example.PowerLevel
import Prelude

import Control.Monad.Eff.Console (log)
import Fusion.Utils (renderToDom)

-- main :: forall eff. RenderedEffect eff
main = renderToDom "app" powerLevelDemo
