module Fusion (
  Component,
  Empty,
  Handle,
  ActionDispatcher,
  Action,
  ReactFx,
  ActionFx,
  ChangeState,
  DomEvent,
  ViewState,
  module Exports,
  hijack,
  class Stateful,
  withState,
  component,
  withDefaults,
  getDefaults
) where

-- Re-exports:
import Prelude

import Control.Monad.Eff (Eff)
import Fusion.Utils (HTML, type ($))
import Fusion.Utils as Exports
import Fusion.Utils.Records (unionMerge)
import Fusion.Utils.Records as Exports
import React (Event, ReactClass, ReactProps, ReactRefs, ReactState, ReactThis, ReadOnly, ReadWrite, createClass, createClassStateless', preventDefault, readState, spec, transformState)

import Fusion.DOM (elements, text, (^), (^^), render)

type Empty = Unit
type Component given children = given -> children -> HTML
type ReactFx eff = Eff (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff) Unit

type ActionFx eff = ReactFx eff
type ChangeState eff = ReactFx eff

type TriggerAction input eff = input -> ReactFx eff

type ActionDispatcher action eff = action -> ChangeState eff
type Action action = forall eff. action -> ChangeState eff
type DomEvent eff = Event -> ChangeState eff

type Handle action state = action -> state -> state

type ViewState state action eff result = state -> ActionDispatcher action eff -> result

-- | Automagically preventDefault
-- | Use like `button ! onClick $ hijackEvent \event -> doSomethingWith event`
hijack fn = \event -> do
  preventDefault event
  _ <- fn event
  pure unit

-- | Internal: Allows a render function to be called from within a React component
-- | by converting ReactChildren to SpaceElements, and returning the
-- | output of renderFn as ReactChildren again.
wrapRenderer renderFn =
  \props children -> render $ renderFn props $ elements children

-- | Internal: Takes a React component and gives it the ability to lift
-- | into a SpaceElements free monad
makeCollector cmp =
  \props children -> cmp ^^ props $ children

-- | Internal: Kinda like wrapRenderer, but different?  Not really.
encaseInReactView original props children =
  render $ original props $ elements children

-- | Internal: Makes a dispatcher for a stateful component
makeDispatcher :: forall props action state eff.
                  ReactThis props state ->
                  Handle action state ->
                  ActionDispatcher action eff
makeDispatcher meta handler = \action -> transformState meta $ handler action

class Stateful (state :: # Type) (action :: Type) (subset :: # Type) | state -> subset, subset -> state where
  withState :: forall givenProps allProps eff.
               (Record subset) ->
               Handle action (Record subset) ->
               ViewState (Record subset) action eff HTML ->
               HTML

instance statefulREcord :: Stateful state action state where
  withState initialState handler viewFn =
    statefulWrapper ^ unit where
      statefulWrapper :: ReactClass Unit
      statefulWrapper = createClass $ spec initialState $ renderFn
      renderFn meta = do
        let dispatch = makeDispatcher meta handler
        state <- readState meta
        pure $ render $ viewFn state dispatch

-- | Creates a real react component, living, breathing, and probably dangerous!
component = makeCollector <<< createClassStateless' <<< wrapRenderer

withDefaults = unionMerge

getDefaults :: forall props defaults combined.
               Union defaults props combined =>
               ((Record props) -> (Record defaults)) ->
               (Record props) ->
               (Record combined)
getDefaults getterFn props = unionMerge computedDefaults props where
  computedDefaults = getterFn props
