module Fusion.Utils (
  SpaceElements,
  ReactChildren,
  RenderedEffect,
  class HasEmpty,
  empty,
  whenever,
  renderToDom,
  class Genum,
  allEnums,
  asText,
  showSpaced,
  showDasherised,
  getStringifiedEnums,
  TypeApply,
  type ($)
) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window) as Browser
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document) as Browser
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), documentToNonElementParentNode)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (class GenericBottom, genericBottom)
import Data.Generic.Rep.Enum (class GenericEnum, genericSucc)
import Data.Generic.Rep.Show (class GenericShow, genericShow)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as Strings
import Data.String.Extra (kebabCase, words)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import React (ReactElement, createClassStateless, createFactory)
import React.DOM (div') as React.DOM
import React.Spaces (SpaceM, renderIn)
import ReactDOM (render) as React.DOM

type SpaceElements = SpaceM
type ReactChildren = Array ReactElement
type RenderedEffect eff = Eff (console :: CONSOLE, dom :: DOM | eff) Unit

-- | Will take an element ID and a SpaceElements node and render it to DOM
renderToDom :: forall e. String -> SpaceElements -> RenderedEffect e
renderToDom selectorId view = do
  let topComponent = createClassStateless \_ -> renderIn React.DOM.div' view
  htmlDocument <- Browser.window >>= Browser.document

  let document = htmlDocumentToDocument htmlDocument

  maybeContainer <- getElementById (ElementId selectorId) $ documentToNonElementParentNode document

  case maybeContainer of
    Just container -> do
      void $ React.DOM.render (createFactory topComponent unit) container
    Nothing -> log "container #app not found"

type TypeApply f a = f a
infixr 0 type TypeApply as $

class HasEmpty a where
  empty :: a
instance emptyString :: HasEmpty String where empty = ""
instance emptyNum :: HasEmpty Number where empty = 0.0
instance emptyInt :: HasEmpty Int where empty = 0
instance emptyArray :: HasEmpty (Array a) where empty = []
instance emptyMaybe :: HasEmpty (Maybe a) where empty = Nothing
instance emptyList :: HasEmpty (List a) where empty = Nil
instance emptyApplicative :: Applicative a => HasEmpty (a Unit) where empty = pure unit

whenever :: forall a. HasEmpty a => Boolean -> a -> a
whenever predicate value = if predicate then value else empty

-- | The power of genericssssss

class Genum a where
  allEnums :: Array a
  asText :: a -> String

diag a = Tuple a a

instance genericEnumInst :: (Generic a b, GenericEnum b, GenericBottom b, GenericShow b) => Genum a where
  allEnums = unfoldr (map diag <<< genericSucc) $ genericBottom
  asText = genericShow

getStringifiedEnums :: forall a rep fun. Genum a => Functor fun => fun a -> fun (Tuple a String)
getStringifiedEnums = map enumAndString where
  enumAndString item = Tuple item (showSpaced $ asText item)

showDasherised :: String -> String
showDasherised = kebabCase

showSpaced :: String -> String
showSpaced = joinWith " " <<< words

joinWith :: forall f. Foldable f => String -> f String -> String
joinWith separator = Strings.joinWith separator <<< fromFoldable
