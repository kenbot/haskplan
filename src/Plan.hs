module Plan where
import Fact
import Data.Monoid


type Name = String
type Time = Int

data Action = Action
  { actionName :: Name 
  , actionCost :: Time 
  , preconditions :: Facts
  , results :: Facts
  } deriving (Eq)


instance Show Action where
  show = actionName 

newtype Plan = Plan 
  { planActions :: [Action] } 
  deriving (Eq, Show)

instance Monoid Plan where
  mappend = addPlan 
  mempty = emptyPlan


childPlans :: [Action] -> Plan -> [Plan]
childPlans knownActions plan = fmap (`addAction` plan) knownActions

addPlan :: Plan -> Plan -> Plan
addPlan (Plan as) (Plan bs) = Plan $ as ++ bs

emptyPlan = Plan []

addAction :: Action -> Plan -> Plan
addAction a (Plan as) = Plan $ as ++ [a] 

planPreconditions :: Plan -> Facts
planPreconditions = maybe emptyFacts preconditions . firstAction . planActions
  where firstAction :: [Action] -> Maybe Action 
        firstAction []    = Nothing
        firstAction (a : _) = Just a

planResults :: Plan -> Facts
planResults = maybe emptyFacts results . lastAction . planActions
  where lastAction []       = Nothing
        lastAction (a : []) = Just a
        lastAction (_ : as) = lastAction as

