module FillInThingy where

import Control.Monad.Eff.Console
import Prelude

import Data.DateTime.Instant (instant)
import Data.Maybe (Maybe(..))
import Data.Record as Rec
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (class ListToRow, class RowToList, Cons, Nil, kind RowList)
import Type.Row as R
import Unsafe.Coerce (unsafeCoerce)

-- | A typeclass that allows defaulting of options, where options that are not
-- | specified in `partial` are filled in with defaults. For each value that
-- | *does* exist in `partial`, it must either have the same type as the
-- | corresponding row in `filledIn` or be a `Maybe` wrapper around that type.
-- (flip fillIn {}) == id
class FillIn
  (partial :: Type) -- Record
  (defaultOptions :: Type) -- Record
  (combined :: Type)
  where
    fillIn :: defaultOptions -> partial -> combined

-- | An unsafe version of fillIn that leaves the missing fields blank.
-- unsafeFillIn :: forall p f. FillIn p f => p -> f
-- unsafeFillIn = fillIn (unsafeCoerce {})

instance fillInInstance ::
    -- given :: Row <-> parts :: RowList
  ( R.RowToList given givenList
  , R.ListToRow givenList given
    -- defaultOptions :: Row <-> defaultList :: RowList
  , R.RowToList defaultOptions defaultList
  , R.ListToRow defaultList defaultOptions
    -- dispatch to the RowList implementation of this
  , FillInImpl givenList given defaultList defaultOptions combined
  ) => FillIn (Record given) (Record defaultOptions) (Record combined)
  where
    fillIn dfs part = fillInImpl
      (R.RLProxy :: R.RLProxy givenList)
      (R.RLProxy :: R.RLProxy defaultList)
      part dfs

-- | If there is a label Cons'ed on top of `iter`, ensure it is on top of `o`
-- | too (where `i` and `o` represent equivalent rows). This exposes the type
-- | of the matching row of `i` in the head of `o` for instance matching.
class ShuffleNextToTop
  (iter :: R.RowList)
  (i :: R.RowList)
  (o :: R.RowList)
  | iter -> i o
instance noShuffle :: ShuffleNextToTop R.Nil i i
instance shuffleNext ::
    -- convert i to row ir
  ( R.ListToRow i ir
    -- remove symbol
  , RowCons sym t2 or ir
    -- convert to rowlist o
  , R.RowToList or o
  ) => ShuffleNextToTop
    -- get the top
    (R.Cons sym t1 rest)
    -- get the remaining rows
    i
    -- and add back on top
    (R.Cons sym t2 o)

-- | Expands iteration over `RowLists` to build the result options.
class FillInImpl
  (iter :: R.RowList)
  (given :: # Type)
  (defaultList :: R.RowList)
  (defaultOptions :: # Type)
  (result :: # Type)
  | iter defaultList -> defaultOptions given result
  , iter -> given, defaultList -> defaultOptions
  where
    fillInImpl :: R.RLProxy iter ->
                  R.RLProxy defaultList ->
                  Record given ->
                  Record defaultOptions ->
                  Record result

-- | If there is no more explicit options left, return what remainds of the
-- | defaults (not including any consumed explicit options).
instance fillInNil ::
  FillInImpl R.Nil () defaultL remaining remaining where
    fillInImpl _ _ _{-{}-} dfs = dfs

-- | If a partial option matches the type of the full option, insert it into
-- | the result.
instance fillInConsZExists ::
    -- ensure it exists in the result
  ( RowCons sym t poppedDefaults defaults
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
  , FillInImpl rest poppedGiven defaultsList poppedDefaults poppedResult
    -- at this key from partial to the result
  , RowCons sym t poppedResult result
  , RowCons sym t poppedGiven given
  , IsSymbol sym
  , R.RowLacks sym poppedGiven
  , R.RowLacks sym poppedDefaults
  , R.RowLacks sym poppedResult
  ) => FillInImpl (R.Cons sym t rest) given (R.Cons sym t defaultsList) defaults result
  where
    fillInImpl _ _ part dfs = Rec.insert key val recurse
      where
        key = SProxy :: SProxy sym
        val :: t
        val = Rec.get key part
        subpart :: Record poppedGiven
        subpart = Rec.delete key (part :: Record given)
        fillInImpl' :: Record poppedGiven -> Record poppedDefaults -> Record poppedResult
        fillInImpl' = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy defaultsList)
        recurse :: Record poppedResult
        recurse = fillInImpl' subpart $ Rec.delete key dfs

instance fillInConsZExistsMandatory ::
    (-- ensure it exists in the result
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
    FillInImpl rest poppedGiven defaultsList defaults poppedResult
    -- at this key from partial to the result
  , RowCons sym t poppedResult result
  , RowCons sym t poppedGiven given
  , IsSymbol sym
  , R.RowLacks sym poppedGiven
  , R.RowLacks sym defaults
  , R.RowLacks sym poppedResult
  ) => FillInImpl (R.Cons sym t rest) given defaultsList defaults result
  where
    fillInImpl _ _ part dfs = Rec.insert key val recurse
      where
        key = SProxy :: SProxy sym
        val :: t
        val = Rec.get key part
        subpart :: Record poppedGiven
        subpart = Rec.delete key (part :: Record given)
        fillInImpl' :: Record poppedGiven -> Record defaults -> Record poppedResult
        fillInImpl' = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy defaultsList)
        recurse :: Record poppedResult
        recurse = fillInImpl' subpart dfs

-- | If a partial option exists as a Maybe whose argument matches the type of
-- | the full option, insert this value or the default value into the result.
instance fillInConsMaybe ::
    -- ensure it exists in the result
  ( RowCons sym t defaults' defaults
    -- recurse through the remaining keys, subtracting from defaults
    -- and building a subresult
  , FillInImpl rest partial' dLJ defaults' result'
  , ShuffleNextToTop rest dL dLJ
  , FillInImpl rest partial' dLN defaults result
  , ShuffleNextToTop rest (R.Cons sym t dL) dLN
    -- at this key from partial to the result
  , RowCons sym t result' result
  , RowCons sym (Maybe t) partial' partial
  , IsSymbol sym
  , R.RowLacks sym partial'
  , R.RowLacks sym defaults'
  , R.RowLacks sym result'
  ) => FillInImpl (R.Cons sym (Maybe t) rest) partial (R.Cons sym t dL) defaults result
  where
    fillInImpl _ _ part dfs =
      case Rec.get key part of
        Nothing -> fillInImplN subpart dfs
        Just val ->
          let
            recurse :: Record result'
            recurse = fillInImplJ subpart $ Rec.delete key dfs
          in Rec.insert key val recurse
      where
        key = SProxy :: SProxy sym
        subpart :: Record partial'
        subpart = Rec.delete key (part :: Record partial)
        fillInImplJ :: Record partial' -> Record defaults' -> Record result'
        fillInImplJ = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy dLJ)
        fillInImplN :: Record partial' -> Record defaults -> Record result
        fillInImplN = fillInImpl
          (R.RLProxy :: R.RLProxy rest)
          (R.RLProxy :: R.RLProxy dLN)

thingy :: { isActive :: Boolean, age :: Int, name :: String }
thingy = fillIn { isActive: false, age: 25 } { isActive: true, name: "Bob" }

main = show $ thingy.isActive

  -- Records where the fields aren't all `FieldType` are an error
  -- log <<< _.foo <<< _.answer $ id' { answer: { bar: 42
  --                                            }
  --                                  }

  -- Non-records are an error
  -- log <<< id' $ "Hello, World"
