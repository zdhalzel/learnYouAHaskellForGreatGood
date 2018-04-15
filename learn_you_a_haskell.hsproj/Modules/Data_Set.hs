module Modules.Data_Set where
  
import qualified Data.Set as Set
import Data.List

setNub = Set.toList . Set.fromList