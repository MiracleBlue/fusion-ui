module Fusion (
  Component,
  ActionHandler,
  ActionDispatcher,
  ChangeState,
  Dispatchable,
  Action,
  ViewState,
  Defaults,
  module Exports,
  hijack,
  withState,
  component
) where

-- Re-exports:
import Prelude

import Control.Monad.Eff (Eff)
import Fusion.Utils (SpaceElements, ReactChildren, RenderedEffect, renderToDom) as Exports
import Fusion.Utils (SpaceElements, type ($))
import Fusion.Utils.Records (class GivenProps, class IntersectRow, class Props, combineGiven, combineProps, unionMerge, unsafeMerge)
import Fusion.Utils.Records (class GivenProps, class Props, unsafeMerge, mapDefaults) as Exports
import React (GetInitialState, ReactClass, ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, createClass, createClassStateless', getChildren, getProps, preventDefault, readState, spec, spec', transformState)
import React.Spaces (elements, text, (^), (^^))
import React.Spaces (render) as RS

type Component given = given -> SpaceElements -> SpaceElements

type ChangeState eff = Eff (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff) Unit

-- | Unused at this moment, kept for... reasons.
type Action input = forall eff. input -> ChangeState eff

type ActionHandler action state = action -> state -> state

type ActionDispatcher action = forall eff. action -> ChangeState eff

type Dispatchable action = ( dispatch :: ActionDispatcher action )



type ViewState state action = state -> ActionDispatcher action -> SpaceElements

renderSpaces = RS.render

-- | Automagically preventDefault
-- | Use like `button ! onClick $ hijackEvent \event -> doSomethingWith event`
hijack fn = \event -> do
  preventDefault event
  pure $ fn event

-- | Internal: Allows a render function to be called from within a React component
-- | by converting ReactChildren to SpaceElements, and returning the
-- | output of renderFn as ReactChildren again.
wrapRenderer renderFn =
  \props children -> renderSpaces $ renderFn props $ elements children

-- | Internal: Takes a React component and gives it the ability to lift
-- | into a SpaceElements free monad
makeCollector cmp =
  \props children -> cmp ^^ props $ children

-- | Internal: Kinda like wrapRenderer, but different?  Not really.
encaseInReactView original props children =
  renderSpaces $ original props $ elements children

-- | Internal: Makes a dispatcher for a stateful component
makeDispatcher :: forall props action state eff.
                  ReactThis props state ->
                  ActionHandler action state ->
                  ActionDispatcher action
makeDispatcher meta handler = \action -> transformState meta $ handler action

-- type StateView state action = SpaceElements

-- | Internal: Actually generates a Stateful react component, taking a Handler function,
-- | an initial state value, and the original Component.
statefulComponent :: forall state action eff.
                     -- handler
                     ActionHandler action (Record state) ->
                     -- initialState
                     Record state ->
                     -- viewFn
                     ViewState (Record state) action ->
                     -- (Record state -> ActionDispatcher action eff -> SpaceElements) ->
                     -- returns:
                     SpaceElements
statefulComponent handler initialState viewFn =
  statefulWrapper ^ unit
  where
    statefulWrapper :: ReactClass Unit
    statefulWrapper = createClass $ spec initialState $ statefulRenderFn
    statefulRenderFn meta = do
      state <- readState meta

      let dispatch = makeDispatcher meta handler
      pure $ renderSpaces $ viewFn state dispatch

-- | Use dis one, not da one above, unless you really want.  I'm a comment, not a cop.
withState = statefulComponent

type Defaults (vessel :: Type -> Type) =
  forall given. vessel (Record given)

-- | Creates a real react component, living, breathing, and probably dangerous!
component = makeCollector <<< createClassStateless' <<< wrapRenderer

-- makeWinning :: forall defaults mandatory given combined.
--                Record defaults ->
--                Component (Record combined) ->
--                Complete Component combined
-- makeWinning defaults view = combineProps defaults >>> view

-- tabItemTwo :: Complete Component ( isActive :: Boolean, name :: String )
-- tabItemTwo = unionMerge { isActive: false } >>> view where
--     view { name, isActive } renderChildren = do
--       text $ "theTab " <> name

-- tabItemTwo :: Defaults Component ( isActive :: Boolean ) ( name :: String )
-- tabItemTwo = combineProps { isActive: false } >>> view where
--     view { name, isActive } renderChildren = do
--       text $ "theTab " <> name

-- whatTheHell :: SpaceElements
-- whatTheHell = tabItemTwo { name: "meow" } do text "What"
