module Fusion.Utils.Records (
  class Optional,
  class Options,
  unionMerge
) where

class Optional (possibilities :: # Type) (subset :: # Type) | possibilities -> subset
instance optionalUnion :: Union subset delta possibilities => Optional possibilities subset

class Options allOptions (subsetOptions :: # Type) | allOptions -> subsetOptions
instance optionsUnion
  :: (
    Union subsetOptions part allOptions
  ) => Options (Record allOptions) subsetOptions

foreign import unsafeMerge :: forall a b c. Record a -> Record b -> Record c

unionMerge :: forall a b c. Union a b c => Record a -> Record b -> Record c
unionMerge = unsafeMerge
