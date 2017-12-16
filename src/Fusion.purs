module Fusion where

import Control.Monad.Eff (Eff)
import Control.Monad.Free (Free)
import Data.Record.Builder as Builder
import Prelude hiding (div)
import React (ReactClass, ReactElement, ReactProps, ReactRefs, ReactState, ReadOnly, ReadWrite, createClass, createClassStateless', getChildren, getProps, preventDefault, readState, spec, transformState)
import React.DOM.Props (className, onClick)
import React.Spaces (SpaceF(..), SpaceM, elements, text, (!), (^^))
import React.Spaces as RS
import React.Spaces.DOM (button, div, span)
import Type.Equality (class TypeEquals)

renderSpaces = RS.render

class Optional (possibilities :: # Type) (subset :: # Type)
instance srInst :: Union subset delta possibilities => Optional possibilities subset

class Props (mandatory :: # Type) (defaults :: # Type) (givenProps :: # Type)
instance propsWithOptionals :: (Union subset delta defaults, Union subset mandatory givenProps) => Props mandatory defaults givenProps

class IsComponent a

type SpaceElements = SpaceM
type SpaceNode = SpaceElements -> SpaceElements
type ReactChildren = Array ReactElement

type ComponentMeta props = { props :: props, renderChildren :: SpaceElements }

type ComponentRender props = { props :: props, renderChildren :: SpaceElements } -> SpaceElements


type Component given = given -> SpaceElements -> SpaceElements

type ComponentAndProps (mandatory :: # Type) (defaults :: # Type) (given :: # Type) = Record given -> SpaceElements -> SpaceElements

type ComponentView (props :: # Type) =  Record props -> SpaceElements -> SpaceElements

type ComponentWithDefaults mandatory defaults allProps = forall innerGiven innerAll.
                                                        Props mandatory defaults innerGiven =>
                                                        Union innerGiven defaults allProps =>
                                                        TypeEquals (Record allProps) (Record innerAll) =>
                                                        Record innerGiven -> SpaceElements -> SpaceElements
                                                        -- ComponentAndProps (Record mandatory) (Record defaults) (Record allProps) (Record innerGiven)

merge :: forall combined first last. Union first last combined => { | first } -> { | last } -> { | combined }
merge first last = Builder.build (Builder.merge last) first

infixr 8 merge as ++


-- collectPropsAndChildrenWithDefaults plainReactSpec =
--   \props children -> plainReactSpec ^^ props $ children

-- Props mandatory defaults given =>
collectPropsAndChildren :: forall mandatory defaults given.
                            ReactClass (Record given) ->
                            -- returns
                            ComponentAndProps mandatory defaults given
collectPropsAndChildren plainReactSpec = \props children -> plainReactSpec ^^ props $ children

hijack fn = \event -> do
  preventDefault event
  pure $ fn event


type AsComponent mandatory defaults = forall optionalSubset all given.
              Optional defaults optionalSubset =>
              Union mandatory optionalSubset given =>
              Union optionalSubset mandatory given =>
              Union defaults given all =>
              Union given defaults all =>
              Record defaults ->
              ComponentView all ->
              ComponentAndProps mandatory defaults given

component :: forall mandatory defaults optionalSubset all given.
              Optional defaults optionalSubset =>
              Union mandatory optionalSubset given =>
              Union defaults given all =>
              Record defaults ->
              ComponentView all ->
              ComponentAndProps mandatory defaults given
component defaultProps viewFn = collectPropsAndChildren componentClass
  where
    componentClass = createClassStateless' $ renderWrapper
    renderWrapper :: Record given -> ReactChildren -> ReactChildren
    renderWrapper givenProps children =
      let
        combinedProps :: Record all
        combinedProps = (defaultProps ++ givenProps)
        renderChildren :: SpaceElements
        renderChildren = elements $ children
      in renderSpaces $ viewFn combinedProps renderChildren
-- component :: forall defaults given combined.
--              Union defaults given combined
--           => Union given defaults combined
--           => ComponentRender (Record combined)
--           -> Record defaults
--           -> Component (Record given)
-- component viewFn defaultProps = collectPropsAndChildren componentClass
--   where
--     componentClass = createClassStateless' $ renderWrapper
--     renderWrapper :: Record given -> ReactChildren -> ReactChildren
--     renderWrapper givenProps children =
--       let
--         combinedProps :: Record combined
--         combinedProps = givenProps ++ defaultProps
--         renderChildren :: SpaceElements
--         renderChildren = elements $ children
--         definedProps :: { props :: Record combined, renderChildren :: SpaceElements }
--         definedProps = { props: combinedProps, renderChildren }
--       in renderSpaces $ viewFn definedProps



-- componentSimple :: forall props.
--                    ComponentSimpleRender (Record props)
--                 -> Component (Record props)
-- componentSimple viewFn = collectPropsAndChildren componentClass
--   where
--     componentClass = createClassStateless' $ renderWrapper
--     renderWrapper :: Record props -> ReactChildren -> ReactChildren
--     renderWrapper givenProps children =
--       let
--         renderChildren :: SpaceElements
--         renderChildren = elements $ children
--       in renderSpaces $ viewFn givenProps renderChildren

makeDispatcher meta handler = \action -> transformState meta $ handler action

statefulComponent handler initialState viewFn =
  let
    statefulRenderFn meta = do
      props <- getProps meta
      state <- readState meta
      children <- getChildren meta

      let dispatch = makeDispatcher meta handler

      pure $ renderSpaces $ viewFn $ {
        props,
        state,
        dispatch,
        renderChildren: elements $ children
      }
    statefulWrapper = createClass $ spec initialState $ statefulRenderFn
  in collectPropsAndChildren statefulWrapper {}

type Action input = forall eff. input -> Eff eff Unit

type ReactEffect eff = Eff (props :: ReactProps, refs :: ReactRefs ReadOnly, state :: ReactState ReadWrite | eff) Unit

type ChangeState result eff = ReactEffect eff

type StatefulHandler action combined = action -> combined -> combined
type Dispatcher state action eff = action -> ChangeState (Record state) eff
type Dispatchable state action props eff = { dispatch :: Dispatcher state action eff | props }
type StatefulComponent state action props = props -> SpaceElements -> SpaceElements

-- withState :: forall action state props stateAndProps eff.
--              Union state props stateAndProps
--           -- handler:
--           => StatefulHandler action (Record state)
--           -- initialState:
--           -> Record state
--           -- originalComponent:
--           -> Component (Dispatchable state action stateAndProps eff)
--           -- returns:
--           -> StatefulComponent (Record state) action (Record props)
-- withState handler initialState originalComponent =
--   let
--     viewFn { renderChildren, props, state, dispatch } = do
--       originalComponent ( { dispatch } ++ state ++ props ) $ do renderChildren
--   in statefulComponent handler initialState viewFn


-- ! onClick (\_ -> dispatch IncreasePower)
