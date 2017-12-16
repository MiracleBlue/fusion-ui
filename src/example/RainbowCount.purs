module Example.RainbowCount where

import Fusion (class Optional, ComponentRender, SpaceElements, StatefulComponent, component, withState, (++))
import Prelude hiding (div)
import React.DOM.Props (className, onClick)
import React.Spaces (text, (!))
import React.Spaces.DOM (div, button, span)

data ButtonAction = IncreasePower | DecreasePower

-- type Renderable ( required :: # Type ) ( defaults :: # Type ) optional props =
--   Optional defaults optional =>
--   Union optional required props =>
--   Record props -> String
--
-- class Props essential default ( props :: # Type )
-- instance propsInstance :: (Optional default optionals, Union essential optionals props) => Props (Record essential) (Record default) props


-- type Thingable (things :: # Type) (optional :: # Type) = Optional things optional => Record optional

-- meow :: { name :: String | (forall optional. Optional ( lives :: Int, colour :: String ) optional => optional) }
-- meow :: forall optional. Thingable ( name :: String, lives :: Int ) optional
-- meow :: forall optional. Optional ( colour :: String, lives :: Int ) optional => Record optional
-- meow = { colour: "Blue" }

-- meow :: forall optional props.
--   Props ( name :: String, colour :: String ) ( breed :: String, lives :: Int ) props =>
--   Record props -> String
-- meow things = "What is going on here" <> things.breed

-- Renderable ( name :: String, colour :: String ) ( breed :: String, lives :: Int ) optional props
--
-- withDefaults defaults = _ { defaults = defaults }
-- withState handler state = _ { state = state, handler = handler }
-- asComponent displayName = _ { displayName = displayName }
--
-- rendering v = {
--   render: v
--   displayName: "Unknown"
-- }
--
-- btn =
--   asComponent "button" view
--   $ withState handler { powerLevel: 1234 }
--   $ withDefaults { name: "bob", super: true, whatEven: 5 }
--   where
--     handler IncreasePower state = state { powerLevel = state.powerLevel + 1 }
--     handler DecreasePower state = state { powerLevel = state.powerLevel - 1 }
--
--     view { renderChildren, props: { colour, name, powerLevel, dispatch } } = do
--       div ! className ("power-" <> name) $ do
--         span $ text $ "powerLevel: " <> show powerLevel
--         button ! onClick (\_ -> dispatch IncreasePower) $ do text "Increase Power"
--         button ! onClick (\_ -> dispatch DecreasePower) $ do text "Decrease Power"
--         renderChildren
--
-- buttonPrimary :: forall optional.
--                  Optional ( name :: String, super :: Boolean, whatEven :: Int ) optional
--               => StatefulComponent { powerLevel :: Int } ButtonAction { colour :: String | optional }
-- buttonPrimary =
--   withState handler { powerLevel: 1234 }
--   $ component view { name: "bob", super: true, whatEven: 5 }
--   where
--     handler IncreasePower state = do
--       state { powerLevel = state.powerLevel + 1 }
--     handler DecreasePower state = do
--       state { powerLevel = state.powerLevel - 1 }
--
--     view { renderChildren, props: { colour, name, powerLevel, dispatch } } = do
--       div ! className ("power-" <> name) $ do
--         span $ text $ "powerLevel: " <> show powerLevel
--         button ! onClick (\_ -> dispatch IncreasePower) $ do text "Increase Power"
--         button ! onClick (\_ -> dispatch DecreasePower) $ do text "Decrease Power"
--         renderChildren
-- --
-- powerLevelDemo :: SpaceElements
-- powerLevelDemo = div ! className "im-walking-on-air" $ do
--   span $ do text "wow"
--   buttonPrimary { colour: "green" } $ do
--     span $ do text "yay"
