module Fusion.Example.PowerLevel where

import Prelude hiding (div)
import Fusion
import Fusion.DOM (a, button, div, h2, li, span, ul, blank, text, (!))
import Fusion.DOM.Props (className, href, onClick)

import Data.Array (head)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String.Extra (snakeCase)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))

data HealthcareTabs = HealthInsurance | AgedCare | TravelInsurance
derive instance genericHealthcareTabs :: Generic HealthcareTabs _
derive instance eqHealthcareTabs :: Eq HealthcareTabs

data TabItem tab = TabItem { tab :: tab, name :: String, id :: String, isActive :: Boolean }
type TabItemList tab = Array $ TabItem tab

data TabEvents tab = ChangeTab tab
type TabAction tab eff = tab -> ActionFx eff


asTabItem :: forall tab. Showable tab => Eq tab => tab -> tab -> TabItem tab
asTabItem activeTab tab = TabItem { tab, name, id, isActive } where
  name = asText tab
  id = snakeCase name
  isActive = tab == activeTab

-- allAsTabItems :: forall item. Showable item => Array item -> Array $ TabItem item
-- allAsTabItems = map asTabItem

tabButton :: forall tab eff.
             Component { item :: TabItem tab, activate :: TabAction tab eff } (String -> HTML)
tabButton { item: TabItem { tab, name, id, isActive }, activate } children = do
  a ! className ("tab-item" ++ isActive ? "is-active") ! href ("#" <> id) ! onClick (\_ -> activate tab) $ do
    children name

tabBar :: forall tab opts eff.
          Showable tab => Eq tab =>
          Options { active :: Maybe tab } opts =>
          Component { tabs :: Array tab | opts } $ (TabItem tab -> TabAction tab eff -> HTML)
tabBar =
  getDefaults getFirstTab >>> render where
    getFirstTab = _.tabs >>> head >>> { active: _ } -- This just feels so good and so right
    render { tabs: [] } _ = blank
    render { tabs, active } children = withState { active } handler view where
      handler :: Handle (TabEvents tab) _
      handler (ChangeTab newTab) = _ { active = Just newTab }

      makeTabItems :: tab -> Array tab -> TabItemList tab
      makeTabItems = asTabItem >>> map

      view { active: Nothing } _ = blank
      view { active: Just activeTab } dispatch = renderTabs $ makeTabItems activeTab tabs where
        activate = ChangeTab >>> dispatch
        renderToChildren tab = children tab activate
        renderTabs itemsToRender = do
          ul ! className "tab-bar" $ do
            traverse_ renderToChildren itemsToRender

powerLevelDemo :: HTML
powerLevelDemo = div ! className "im-walking-on-air" $ do
  div do
    text "Hello there!"
  div do
    tabBar { tabs: [HealthInsurance, AgedCare, TravelInsurance] } $
      \item activate -> tabButton { item, activate } do
          \name -> text name
