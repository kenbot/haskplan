module Fact where
import qualified Data.Set as Set
import Data.Monoid

data Fact
  = HasFirewood
  | HasAxe
  | HasLogs
  | HasIronOre
  | AxeAvailable
  deriving (Show, Eq, Ord)

newtype Facts = Facts (Set.Set Fact) 
  deriving (Eq, Show, Ord)

addFact :: Fact -> Facts -> Facts
addFact f (Facts fs) = Facts $ Set.insert f fs

factOf :: Fact -> Facts
factOf = Facts . Set.singleton 

isTrue :: Fact -> Facts -> Bool
isTrue f (Facts fs) = Set.member f fs

removeFact :: Fact -> Facts -> Facts
removeFact f (Facts fs) = Facts $ Set.delete f fs

addFacts :: Facts -> Facts -> Facts
addFacts (Facts fs1) (Facts fs2) = Facts $ Set.union fs1 fs2

includesFacts :: Facts -> Facts -> Bool
includesFacts (Facts inner) (Facts outer) = inner `Set.isSubsetOf` outer 

emptyFacts = Facts Set.empty

instance Monoid Facts where
  mappend = addFacts
  mempty = emptyFacts


