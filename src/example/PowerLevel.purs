module Fusion.Example.PowerLevel where

import Prelude hiding (div,when)

import Control.Alt ((<|>))
import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, execState, state)
import Data.Array (head, (!!))
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (traverse_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Fusion (class GivenProps, Action, ActionHandler, Dispatchable, SpaceElements, ViewState, hijack, withState)
import Fusion.Utils (type ($), allEnums, asText, getStringifiedEnums, showSpaced, class Genum)
import Fusion.Utils.Records (class Props, class RecordMerge, class Subrow, class UnionNub, unsafeMerge)
import React (Event)
import React.DOM.Props (className, onClick)
import React.Spaces (empty, text, (!))
import React.Spaces.DOM (button, div, span, ul, li, a)
import Type.Equality (class TypeEquals)
import Type.Row (class ListToRow, class RowListAppend, class RowToList, RProxy(..), kind RowList)
import Unsafe.Coerce (unsafeCoerce)
import FillInThingy (class FillIn, fillIn)

data Events = Increment | Decrement
data TabBarEvents tab = ChangeTab tab

data Tab = Home | AboutMe | PureScriptIsBestScript | PlzVolvoGiveDireTide
derive instance genericTab :: Generic Tab _
derive instance eqTab :: Eq Tab

type HTML = SpaceElements

allTabs :: Array Tab
allTabs = allEnums

type ViewFn props = props -> HTML -> HTML
type Givens all = forall defaults given. Union defaults given all => Record given -> HTML -> HTML

type Component allProps defaults givenProps = {
  defaults :: Record defaults,
  renderFn :: (Record allProps -> SpaceElements -> SpaceElements),
  view :: (Record givenProps -> HTML -> HTML)
}

class MergeRowLists (firstRL :: RowList) (lastRL :: RowList)

class MergeRows (first :: # Type) (last :: # Type) (result :: # Type) | first last -> result
instance mergeRowsRecords :: (
    RowToList first firstList,
    RowToList last lastList,
    RowListAppend firstList lastList resultList,
    ListToRow resultList result
  ) => MergeRows first last result
-- data Component allProps = Component {
--   renderFn :: ViewFn allProps,
--   view :: forall given. ViewFn (Record given)
-- }

-- withDefaults :: forall allProps defaultProps giveny.
--                 Union giveny defaultProps allProps =>
--                 Record defaultProps ->
--                 ViewFn (Record allProps) ->
--                 (Record giveny -> HTML -> HTML)
-- withDefaults defaults renderFn = view where
--   view :: ViewFn (Record giveny)
--   view given = renderFn $ unsafeMerge defaults given
-- withDefaults :: forall allProps defaultProps givenProps.
--                 MergeRows defaultProps givenProps allProps =>
--                 Record defaultProps ->
--                 Component allProps _ _ ->
--                 Component allProps defaultProps givenProps
-- withDefaults defaults { renderFn } = {
--   view,
--   defaults,
--   renderFn
-- } where
--   view :: forall givenPropsView. ViewFn (Record givenPropsView)
--   view given = renderFn $ unsafeMerge defaults given
--
-- component :: forall given allProps.
--              (Record allProps -> HTML -> HTML) ->
--              Component allProps () given
-- component renderFn = { renderFn, view, defaults: {} } where
--   view :: Record given -> HTML -> HTML
--   view given = renderFn $ unsafeMerge {} given
-- --
-- lolItem :: forall what. Show what => ViewFn {
--   isActive :: Optional Boolean,
--   name     :: Optional String,
--   tab      :: Mandatory what,
--   age      :: Mandatory Int,
--   status   :: Mandatory String
-- }
-- lolItem = withOptionals {
--   isActive: Default false,
--   name: Default "Bob John",
--   tab: Mandatory,
--   age: Mandatory,
--   status: Mandatory
-- } do text (name <> show tab)
--
-- -- tabItem :: Component (forall what. Show what => { isActive :: Boolean, name :: String, tab :: what } -> HTML -> HTML)
-- -- tabItem :: forall what. Show what => Component { isActive :: Boolean, name :: String, tab :: what }
-- tabItem :: forall what. Show what => Component _ _ _
-- tabItem = withDefaults { isActive: false } $ component renderFn where
--   renderFn :: ViewFn { isActive :: Boolean, name :: String, tab :: what }
--   renderFn { isActive, name, tab } chldrn = do text (name <> show tab)
--
--
-- -- tabItem :: forall what. Show what => Givens ( isActive :: Boolean, name :: String, tab :: what )
-- -- tabItem = withDefaults { isActive: false } renderFn where
-- --   renderFn :: ViewFn { isActive :: Boolean, name :: String, tab :: what }
-- --   renderFn { isActive, name, tab } chldrn = do text (name <> show tab)
--

tabItem = fillIn { isActive: false, age: 15 } >>> view where
  view :: { isActive :: Boolean, age :: Int, name :: String } -> HTML -> HTML
  view { isActive, age, name } childrens = do
    text $ "Hello, " <> name <> "!  You are " <> show age <> " years old."

powerLevelDemo :: SpaceElements
powerLevelDemo = div ! className "im-walking-on-air" $ do
  span do
    text "wow"
    tabItem { name: "Dave", isActive: true } do empty
    -- lol $ _ { tab = Home, isActive = true }
    -- tabItem.view { name: "meow", tab: 123 } do text "nothing"
    -- tabBarAgain { tabs: [Home, AboutMe, PureScriptIsBestScript], activeTab: Home } do text "nothing"
    -- tabBar { tabs: [Home, AboutMe, PureScriptIsBestScript], activeTab: Home } do
    --   text "yellow"
