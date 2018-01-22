module Fusion.Utils.Records (
  class Optional,
  class RecordMerge,
  class Subrow,
  class IntersectRow,
  class UnionNub,
  class Props,
  combineProps,
  class GivenProps,
  combineGiven,
  mapDefaults,
  mergeProps,
  realMerge,
  defaultsMerge,
  unionMerge,
  unsafeMerge
) where

import Prelude

import Type.Row (class ListToRow, class RowListNub, class RowToList, RProxy(..))
import Unsafe.Coerce (unsafeCoerce)

{-
instance whatTheFuck
  :: (
    -- defaults  |  alpha   -> beta
    -- required  |  alpha   -> charlie
    -- missing   |  beta    -> delta
    -- provided  |  charlie -> delta
    -- given     |  beta    -> charlie
    -- result    |  alpha   -> delta
    Union defaults  required  result,   -- alpha
    Union given     missing   defaults, -- beta
    Union required  given     provided, -- charlie
    Union provided  missing   result    -- delta
  ) => AddDefaults defaults provided result
  where
    addDefaults defaults provided =
      merge defaults provided
-}

class UnionNub (original :: # Type) (possibleDuplicates :: # Type) (dedupedResult :: # Type) | original possibleDuplicates -> dedupedResult where
  realMerge :: Record original -> Record possibleDuplicates -> Record dedupedResult

instance unionNub
  :: (
    Union possibleDuplicates original unionedRow,
    RowToList unionedRow unionedRowList,
    RowListNub unionedRowList dedupedRowList,
    ListToRow dedupedRowList dedupedResult
  ) => UnionNub original possibleDuplicates dedupedResult
 where realMerge = unsafeMerge >>> compose (unsafeCoerce :: Record unionedRow -> Record dedupedResult)

class Optional (possibilities :: # Type) (subset :: # Type)
instance optionalUnion :: Union subset delta possibilities => Optional possibilities subset

-- | Proof that row `subset` is a subset of row `original`
class Subrow (subset :: # Type) (original :: # Type) | original -> subset
instance omgWowSubrows :: Union subset delta original => Subrow subset original

-- | Proof of row `intersection` being the intersection of rows `first` and `last`
class IntersectRow (combined :: # Type) (given :: # Type) (mandatory :: # Type) (defaults :: # Type) (givenOfDefaults :: # Type) | defaults mandatory givenOfDefaults -> combined given, defaults given givenOfDefaults -> combined mandatory

--             ( mandatory: { b :: Bool, c :: String } )
-- first:         { a :: Int, b :: Bool, c :: String }  (combined)
-- deltaFirst:    { a :: Int }                          (default)
-- intersection:                       { c :: String }
-- deltaLast:               { b :: Bool }               (subset of default)
-- last:                    { b :: Bool, c :: String }  (given)
instance intersectRowInstance
  :: (
    Union defaults mandatory combined,
    Union mandatory givenOfDefaults given
  ) => IntersectRow combined given mandatory defaults givenOfDefaults

------------------------------------------------------------------------------

class Props (defaults :: # Type) (mandatory :: # Type) (given :: # Type) (combined :: # Type)
  | defaults mandatory -> given combined where
  -- meow :: forall m. m mandatory
  combineProps :: RProxy mandatory -> Record defaults -> Record given -> Record combined

instance omgWowAmazing
  :: (
    IntersectRow combined given mandatory defaults givenOfDefaults,
    Subrow givenOfDefaults defaults
  ) => Props defaults mandatory given combined
 where
  combineProps _ defaults given = unsafeMerge defaults given



------------------------------------------------------------------------------

class GivenProps (defaults :: # Type) (given :: # Type) combined | defaults given -> combined where
  combineGiven :: Record defaults -> Record given -> Record combined

instance omgWowAmazing1
  :: (
    IntersectRow combined given mandatory defaults givenOfDefaults,
    Subrow givenOfDefaults defaults
  ) => GivenProps defaults given combined
 where combineGiven = unsafeMerge

-- withDefaults :: Record defaults -> vessel (Record combined) -> vessel (Record given)
mergeProps :: forall defaults given combined mandatory.
              Props defaults mandatory given combined =>
              Record defaults -> Record given -> Record combined
mergeProps = unsafeMerge

mapDefaults :: forall defaults mandatory given all.
               GivenProps defaults mandatory given =>
               (Record given -> Record defaults) ->
               Record given ->
               Record all
mapDefaults fn given = unsafeMerge (fn given) given

------------------------------------------------------------------------------

class RecordMerge (defaults :: # Type) (given :: # Type) (combined :: # Type) | defaults given -> combined

instance recordMergeIntersect
  :: (
    IntersectRow combined given mandatory defaults givenOfDefaults,
    Subrow givenOfDefaults defaults
  ) => RecordMerge defaults given combined

------------------------------------------------------------------------------

-- | Merge any two records together unsafely.
-- | Fields common between the two will result in the value from r2 being kept
foreign import unsafeMerge
  :: forall r1 r2 r3
  .  Record r1
  -> Record r2
  -> Record r3

-- | Merge a record `mr` with optional default values `o`, resulting in record `mo`.
-- |
-- | The record `mr` must consist of the common fields from `mo` and `mr` plus a subset
-- | of fields from `o`.
-- |
-- | Examples:
-- | * `merge {a:1,b:"Unspecified"} {a:3,c:"Mandatory"} = {a:3,b:"Unspecified",c:"Mandatory"}`
-- | * `merge {a:1,b:"Unspecified"} {c:"Only mandatory"} = {a:1,b:"Unspecified",c:"Only Mandatory"}`
-- | * `merge {a:1,b:"Unspecified"} {a:"Wrong type"} = wont compile`
defaultsMerge :: forall defaults given combined. RecordMerge defaults given combined => Record defaults -> Record given -> Record combined
defaultsMerge = unsafeMerge

unionMerge :: forall a b c. Union a b c => Record a -> Record b -> Record c
unionMerge = unsafeMerge
