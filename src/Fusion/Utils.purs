module Fusion.Utils where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Eff
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Maybe (Maybe(..))

import React (createClassStateless, createFactory)
import ReactDOM (render)
import React.DOM as React.DOM
import Fusion (SpaceElements)
import React.Spaces (renderIn)

type RenderedEffect eff = Eff (console :: CONSOLE, dom :: DOM | eff) Unit

renderToDom :: forall e. String -> SpaceElements -> RenderedEffect e
renderToDom selectorId view = do
  let topComponent = createClassStateless \_ -> renderIn React.DOM.div' view
  htmlDocument <- DOM.window >>= DOM.document

  let document = htmlDocumentToDocument htmlDocument

  maybeContainer <- getElementById (ElementId selectorId) $ documentToNonElementParentNode document

  case maybeContainer of
    Just container -> do
      void $ render (createFactory topComponent unit) container
    Nothing -> Eff.log "container #app not found"
