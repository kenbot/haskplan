{-# LANGUAGE LambdaCase #-}
module Main where
import Fact
import Plan



main :: IO ()
main = putStrLn "We're live!"


data Agent = Agent 
  { agentFacts :: Facts
  , agentKnownPlans :: [Plan] 
  } deriving (Show)



allowedActions :: Facts -> [Action] -> [Action]
allowedActions currentFacts = 
  let meetsPreconditions = (flip includesFacts) currentFacts . preconditions
  in filter meetsPreconditions 

actionsThatCanSatisfy :: Facts -> [Action] -> [Action]
actionsThatCanSatisfy goals = 
  let satisfiesGoals = includesFacts goals . results
  in filter satisfiesGoals 


type Cost = Double

data Node = Node 
  { nodeAction :: Action
  , nodeTotalCost :: Cost
  , nodeResultState :: Facts
  } 
    

getPlans :: [Action] -> Facts -> Facts -> [Plan]
getPlans allActions facts0 goals = 
  let childNodes :: Node -> [Node] 
      childNodes (Node _ cost state) =  
        let allowed = allowedActions state allActions
            newChild a = Node a (cost + 1) (state `addFacts` (results a))
        in map newChild allowed
      growPlans :: Facts -> Cost -> [[Node]] -> [[Node]] -> [Plan]
      -- terminal case = ???
      growPlans facts maxCost openPlans finalPlans = 
        growPlans newFacts (openPlans ++ )
  in growPlans facts0 [] 

{-
      growPlans :: [Fact] -> [Plan] -> [Plan]
      growPlans facts [] finalPlans = finalPlans
      growPlans facts openPlans finalPlans = 
        let newPlans = 
          do 
            a <- allowedActions facts allActions
            p <- openPlan 
            return $ addAction allowed openPlan
           
        in growPlans facts newPlans finalPlans 
-}  


---- REPL fodder
collectBranchesAction = Action "Collect branches" 8 emptyFacts (factOf HasFirewood)
getAxeAction = Action "Get axe" 2 (factOf AxeAvailable) (factOf HasAxe)
chopLogAction = Action "Chop log" 4 (factOf HasAxe) (factOf HasFirewood)

actions :: [Action]
actions = [collectBranchesAction, getAxeAction, chopLogAction]

