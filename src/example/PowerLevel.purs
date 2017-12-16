module Example.PowerLevel where

import Data.Enum (class Enum, enumFromTo)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Show (genericShow)
import Data.String (joinWith)
import Data.String.Extra (kebabCase, words)
import Data.Traversable (traverse)
import Fusion (class Optional, ComponentView, Action, Component, ComponentAndProps, SpaceElements, SpaceNode, StatefulComponent, AsComponent, component, hijack)
import Prelude hiding (div,when)
import React.DOM.Props (className, onClick)
import React.Spaces (text, (!))
import React.Spaces.DOM (button, div, span, ul, li, a)

data Foo = Bar | Baz | Blah | YeahBuddyWassup
derive instance genericFoo :: Generic Foo _

instance showFoo :: Show Foo where
  show x = genericShow x

showDasherised :: forall a. Show a => a -> String
showDasherised = kebabCase <<< show

showSpaced :: forall a. Show a => a -> String
showSpaced = joinWith " " <<< words <<< show

class HasEmpty a where
  empty :: a
instance emptyString :: HasEmpty String where empty = ""
instance emptyApplicative :: Applicative a => HasEmpty (a Unit) where empty = pure unit

when :: forall a. HasEmpty a => Boolean -> a -> a
when predicate value = if predicate then value else empty
--
-- showAsWords :: forall a. Show a => a -> String
-- showAsWords a = words $ show a <$>
--
-- data ButtonAction = IncreasePower | DecreasePower | Meowmeow
--
-- buttonPrimary :: forall optional.
--                  Optional ( name :: String, super :: Boolean, whatEven :: Int ) optional
--               => StatefulComponent { powerLevel :: Int, meowType :: String } ButtonAction { colour :: String | optional }
-- buttonPrimary =
--   withState handler { powerLevel: 6 }
--   $ component view { name: "bob", super: true, whatEven: 5 }
--   where
--     handler IncreasePower state = state { powerLevel = state.powerLevel + 1 }
--     handler DecreasePower state = state { powerLevel = state.powerLevel - 1 }
--     handler Meowmeow state = state
--
--     view { renderChildren, props: { colour, name, powerLevel, dispatch } } = do
--       div ! className ("power-" <> name) $ do
--         span $ text $ "powerLevel: " <> show powerLevel-- <> ", meowType: " <> show meowType
--         button ! onClick (hijack $ \_ -> dispatch IncreasePower) $ do text "Increase Power"
--         button ! onClick (hijack $ \_ -> dispatch DecreasePower) $ do text "Decrease Power"
--         renderChildren
--
-- powerLevelDemo :: SpaceElements
-- powerLevelDemo = div ! className "im-walking-on-air" $ do
--   span $ do text "wow"
--   buttonPrimary { colour: "green" } $ do
--     span $ do text "yay"

data Tab = HealthInsurance | AgedCare | Dentistry | Optical

derive instance genericTab :: Generic Tab _
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab
instance showTab :: Show Tab where
  show = genericShow
instance enumTab :: Enum Tab where
  succ = genericSucc
  pred = genericPred
instance boundedTab :: Bounded Tab where
  bottom = genericBottom
  top = genericTop

allTabs :: Array Tab
allTabs = enumFromTo top bottom

data ChangeTab = ChangeTab Tab

takeTabUpTheFaceHole :: Tab -> ChangeTab
takeTabUpTheFaceHole tab = ChangeTab tab


-- tabBar :: forall optional.
--           Optional ( initialTab :: Tab ) optional
--        => StatefulComponent { selectedTab :: Tab } ChangeTab { | optional }
-- tabBar =
--   withState handler { selectedTab: HealthInsurance }
--   $ component view { initialTab: HealthInsurance }
--   where
--     handler (ChangeTab tab) state = state { selectedTab = tab }
--
--     view { renderChildren, props: { selectedTab, dispatch } } = do
--       let select = dispatch <<< ChangeTab
--       let renderTabItem = \tab -> tabItem li { tab, select, isActive: tab == selectedTab }
--
--       div ! className "tab-bar" $ do
--         ul ! className "tab-bar__tabs" $ do
--           traverse renderTabItem allTabs
--
-- type TabItemOptionals = ( isActive :: Boolean )
-- type TabItemProps r = { tab :: Tab, select :: Action Tab | r }
-- { renderChildren, props: { tab, select, isActive } } = do


-- tabItem :: SpaceNode -> Component ( tab :: Tab, select :: Action Tab ) ( isActive :: Boolean )
-- tabItem :: forall props. ComponentAndProps ( tab :: Tab, select :: Action Tab ) ( isActive :: Boolean ) props
-- view :: Component (Record props)
-- view :: forall combined. Union ( isActive :: Boolean ) ( tab :: Tab, select :: Action Tab ) combined => ComponentView combined
tabItem = tabItemComponent { isActive: false } view where
  tabItemComponent :: AsComponent ( tab :: Tab, select :: Action Tab ) ( isActive :: Boolean )
  tabItemComponent = component
  view { tab, select, isActive } renderChildren = do
    let tabAsWords = select <> "meow"
    let whatever = takeTabUpTheFaceHole tab
    -- let tabClass = "tab-item" <>
    li ! className "tab-item" $ do
      text "tabAsWords"

tabItem :: Component ( tab :: Tab, select :: Action Tab ) ( isActive :: Boolean )
tabItem = component { isActive: false } view where
  view { tab, select, isActive } renderChildren = do
    let tabAsWords = isActive <> "meow"
    li ! className "tab-item" $ do
      text "tabAsWords"


-- tabItem :: ComponentAndProps ( tab :: Tab, select :: Action Tab ) ( isActive :: Boolean ) _
-- tabItem = component { isActive: false } view where
--   view { tab, select, isActive } renderChildren = do
--     let tabAsWords = showSpaced tab
--     li ! className "tab-item" $ do
--       text "tabAsWords"

-- tabbity = (
--     component :: AsComponent ( tab :: Tab, select :: Action Tab ) ( isActive :: Boolean )
--   ) { isActive: false } view where
--     view { tab, select, isActive } children = do
--       li ! className "tab-item" $ do
--         text "Hey there bud"

-- withDefaults defaults = _ { defaults = Just defaults }
-- withState handler initialState = _ { state = Just { handler, value: initialState } }
-- render view = {
--   view,
--   defaults: Nothing,
--   state: Nothing
-- }
--
-- tabItem wrapperNode =
--   asComponent $
--   withDefaults { isActive: true } $
--   withState handler { counter: 0 } $
--   render view where
--     view { counter, tab, isActive, select }

  --in --withDefaults { isActive: false } $ render view


-- tabItem :: forall default mandatory given allProps. Record given -> SpaceElements
-- tabItem wrapperNode =
--   withDefaults { isActive: false } $ view where
--     view { tab, select, isActive } = do
--       let activeClass = when isActive "is-active"
--       let tabAsWords = showSpaced tab
--
--   componentSimple { isActive: false } view where
--     view { tab, select, isActive } renderChildren = do
--       let activeClass = when isActive "is-active"
--       let tabAsWords = showSpaced tab
--       wrapperNode ! className ("tab-item" <> activeClass) $ do
--         a ! className "tab-item__link" $ do
--           text tabAsWords


-- type Props (defaults :: # Type) (mandatory :: # Type) = forall optional props. Union mandatory defaults all => Optional defaults optional => Union mandatory optional props => Component (Record props)

-- buttonSimple :: Props ( name :: String, super :: Boolean, what :: Int ) ( colour :: String )
-- buttonSimple = component view { name: "Joe", super: false, what: 9 }
--   where
--     view { renderChildren, props: { colour, name, super, what } } = do
--       button ! className ( "simple-" <> name ) $ do
--         span $ do text "Hello, this is a span"
--         renderChildren
-- buttonSimple :: Component ( colour :: String ) ( name :: String, super :: Boolean, what :: Int )
-- buttonSimple = component view { name: "Joe", super: false, what: 9 }
--   where
--     view { renderChildren, props: { colour, name, super, what } } = do
--       button ! className ( "simple-" <> name ) $ do
--         span $ do text "Hello, this is a span"
--         renderChildren
