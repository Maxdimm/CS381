-- Name: Rikki Gibson
-- ONID: gibsonri
-- I recognize that I'm defying Haskell conventions by giving
-- function parameters descriptive names instead of a few letters.

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)
import Data.Maybe

import KarelSyntax
import KarelState

getCard :: Robot -> Card
getCard (_, card, _) = card

-- | Valuation function for Test.
-- Test a world and a robot for a given condition.
test :: Test -> World -> Robot -> Bool
test (Not innerTest) world robot =
  not (test innerTest world robot)

test (Facing cardinalDir) _ (_, robotFacingDir, _) =
  cardinalDir == robotFacingDir

test (Clear relativeDir) world (robotPos, robotFacingDir, _) =
  let
    absoluteDir = cardTurn relativeDir robotFacingDir
    neighboringSpace = world (neighbor absoluteDir robotPos)
  in isJust neighboringSpace

test Beeper world (robotPos, _, _) =
  maybe False (> 0) (world robotPos)

test Empty _ (_, _, robotBeepers) = robotBeepers > 0

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ robot = Done robot

stmt Move _ world (robotPos, robotFacingDir, robotBeepers) =
  let
    robot = (robotPos, robotFacingDir, robotBeepers)
    isClear = test (Clear Front) world robot
    nextPos = neighbor robotFacingDir robotPos
  in if isClear
       then OK world (nextPos, robotFacingDir, robotBeepers)
       else Error ("Drove into wall and exploded at position " ++ show robotPos)

stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)

stmt PutBeeper _ world robot =
  let pos = getPos robot in
  let newWorld = incBeeper pos world in
  let newRobot = decBag robot
  in if isEmpty robot
      then Error ("No beeper to put at: " ++ show pos)
      else OK newWorld newRobot

stmt (Turn dir) _ world robot =
  let
    newCard = cardTurn dir (getCard robot)
    newRobot = setFacing newCard robot
  in OK world newRobot

stmt (Call macro) defs world robot =
  case lookup macro defs of
    Just innerStmt -> stmt innerStmt defs world robot
    _ -> Error ("Unknown macro called: " ++ show macro)

stmt (Iterate i innerStmt) defs world robot =
  if i == 0
    then OK world robot
    else case stmt innerStmt defs world robot of
      OK newWorld newRobot ->
        stmt (Iterate (i-1) innerStmt) defs newWorld newRobot
      result -> result

stmt (If cond thenStmt elseStmt) defs world robot =
  if test cond world robot
    then stmt thenStmt defs world robot
    else stmt elseStmt defs world robot

stmt (While cond body) defs world robot =
  if test cond world robot
    then case stmt body defs world robot of
      (OK newWorld newRobot) -> stmt (While cond body) defs newWorld newRobot
      result -> result
    else OK world robot

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
