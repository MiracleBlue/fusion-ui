module Fusion.Utils (
  HTML,
  ReactChildren,
  RenderedEffect,
  class HasEmpty,
  empty,
  renderToDom,
  showSpaced,
  showDasherised,
  TypeApply,
  type ($),
  ReverseTypeApply,
  type (:),
  class Showable,
  toString,
  asWords,
  asKebabCase,
  asPascalCase,
  class CombineSpaced,
  combine,
  combineSpaced,
  (++),
  whenever,
  (?)
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
import Data.String.Extra (kebabCase, pascalCase, words)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import React (ReactElement, createClassStateless, createFactory)
import React.DOM (div') as React.DOM
import React.Spaces (SpaceM, renderIn)
import ReactDOM (render) as React.DOM

type HTML = SpaceM
type ReactChildren = Array ReactElement
type RenderedEffect eff = Eff (console :: CONSOLE, dom :: DOM | eff) Unit

-- | Will take an element ID and a HTML node and render it to DOM
renderToDom :: forall e. String -> HTML -> RenderedEffect e
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
infixr 9 type TypeApply as $

type ReverseTypeApply a f = f a
infixr 9 type ReverseTypeApply as :

class HasEmpty a where
  empty :: a
instance emptyString :: HasEmpty String where empty = ""
instance emptyNum :: HasEmpty Number where empty = 0.0
instance emptyInt :: HasEmpty Int where empty = 0
instance emptyArray :: HasEmpty (Array a) where empty = []
instance emptyMaybe :: HasEmpty (Maybe a) where empty = Nothing
instance emptyList :: HasEmpty (List a) where empty = Nil
instance emptyApplicative :: Applicative a => HasEmpty (a Unit) where empty = pure unit

-- class (Generic a rep, GenericShow rep) <= Showable a rep where
--   asText :: tab -> String
class Showable a where
  toString :: a -> String
  asWords :: a -> String
  asKebabCase :: a -> String
  asPascalCase :: a -> String

instance showableGeneric :: (Generic a rep, GenericShow rep) => Showable a where
  toString = genericShow
  asWords = genericShow >>> showSpaced
  asKebabCase = genericShow >>> kebabCase
  asPascalCase = genericShow >>> pascalCase

showDasherised :: String -> String
showDasherised = kebabCase

showSpaced :: String -> String
showSpaced = joinWith " " <<< words

joinWith :: forall f. Foldable f => String -> f String -> String
joinWith separator = Strings.joinWith separator <<< fromFoldable

joinSpaced = joinWith " "

class CombineSpaced a b where
  combine :: a -> b -> String

instance combineSpacedStrings :: CombineSpaced String String where
  combine first last =          joinSpaced [first, last]

instance combineSpacedMaybeLast :: CombineSpaced String (Maybe String) where
  combine first (Just last) =   joinSpaced [first, last]
  combine first Nothing =       first

instance combineSpacedMaybeFirst :: CombineSpaced (Maybe String) String where
  combine (Just first) last =   joinSpaced [first, last]
  combine Nothing last =        last

instance combineSpacedMaybeBoth :: CombineSpaced (Maybe String) (Maybe String) where
  combine (Just first) last =   combine first last
  combine Nothing (Just last) = last
  combine Nothing Nothing =     ""

combineSpaced = combine
infixr 2 combineSpaced as ++

whenever true value = Just value
whenever false _ = Nothing

infixr 9 whenever as ?

-- Unused code, leaving here for now

--
-- | The power of genericssssss

-- class Genum a where
--   allEnums :: Array a
--   asText :: a -> String
--
--
--
-- instance genericEnumInst :: (Generic a b, GenericEnum b, GenericBottom b, GenericShow b) => Genum a where
--   allEnums = unfoldr (map diag <<< genericSucc) $ genericBottom
--   asText = genericShow
--
-- getStringifiedEnums :: forall a rep fun. Genum a => Functor fun => fun a -> fun (Tuple a String)
-- getStringifiedEnums = map enumAndString where
--   enumAndString item = Tuple item (showSpaced $ asText item)
