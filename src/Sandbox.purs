module Sandbox where

import Data.Record.Builder as Builder
import Prelude hiding (div)
import React (ReactClass, ReactElement, createClassStateless')
import React.DOM.Props (className)
import React.Spaces (SpaceM, elements, text, (!), (^^))
import React.Spaces as RS
import React.Spaces.DOM (div, span)
import Type.Row (class RowToList, class RowListAppend, class ListToRow)

renderSpaces = RS.render

class Merged (f :: # Type) (l :: # Type)
instance flMerged :: Union l f c => Merged f l

type RowAppend (f :: # Type -> # Type) (r :: # Type) = f r

infixr 0 type RowAppend as $

class CombinedRows (first :: # Type) (second :: # Type) (combined :: # Type) 

instance combineRowsFromLists
  :: (
    RowToList first firstList,
    RowToList second secondList,
    RowListAppend firstList secondList combinedList,
    ListToRow combinedList combined
  ) => CombinedRows first second combined

-- type Lulz first second combined = (
--   forall firstList secondList combinedList.
--          RowToList first firstList =>
--          RowToList second secondList =>
--          RowListAppend firstList secondList combinedList =>
--          ListToRow combinedList combined =>
--          Record combined
-- )

-- yay :: forall combined. CombinedRows ( foo :: Int ) ( bar :: String ) combined => Record combined
-- yay = { foo: 5, bar: "Hello" }

class Optional (s :: # Type) (r :: # Type)
instance srInst :: Union r t s => Optional s r

type SpaceChildren = SpaceM
type SpaceElements = SpaceM
type ReactElements = Array ReactElement

type ComponentMeta props = { props :: props, renderChildren :: SpaceElements }
type ComponentRender props = ComponentMeta props -> SpaceElements
type Component given = given -> SpaceElements -> SpaceElements

merge first last = Builder.build (Builder.merge last) first

renderWrapper viewFn defaultProps givenProps children =
  let combinedProps = merge defaultProps givenProps
  in renderSpaces $ viewFn $ {
      props: combinedProps,
      renderChildren: elements $ children
    }

component viewFn defaultProps =
  let
    componentClass = createClassStateless' $ renderWrapper viewFn defaultProps
    collectProps reactSpec = \props children -> reactSpec ^^ props $ children
  in collectProps componentClass

data ButtonAction = ToggleOn | ToggleOff | IncreasePower | DecreasePower

buttonPrimary :: forall optional.
                 Optional ( name :: String ) optional
              => Component { colour :: String  | optional }
buttonPrimary = component view { name: "bob" } where
  view { renderChildren, props: { colour, name } } = do
    div ! className ("button" <> name) $ do
      renderChildren


believeOrNot :: SpaceM
believeOrNot = div ! className "im-walking-on-air" $ do
  span $ do text "wow"
  {-- Will throw an error if you do not include colour, or give name the wrong type, but is fine without name --}
  buttonPrimary { name: "george", colour: "green" } $ do
    span $ do text "yay"
