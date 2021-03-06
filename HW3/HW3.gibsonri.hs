-- Names:
--  Rikki Gibson -- ONID: gibsonri
--  Benjamin Narin -- ONID: narinb
module HW3 where

import MiniMiniLogo
import Render

--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen newMode) (_, point) = ((newMode, point), Nothing)
cmd (Move newX newY) (Down, (x,y)) =
  let
    newState = (Down, (newX, newY))
    line = ((x,y), (newX, newY))
  in (newState, Just line)
cmd (Move newX newY) (Up, _) = ((Up, (newX, newY)), Nothing)

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
-- prog = progAcc []
prog [] state = (state, [])
prog (command:commands) state =
  let (newState, maybeLine) = cmd command state
      (finalState, restOfLines) = prog commands newState
  in (finalState, maybe restOfLines (: restOfLines) maybeLine)

sizableBox :: Int -> Int -> Int -> Int -> Prog
sizableBox x y w h = [Pen Up, Move x y, Pen Down,
           Move (x+w) y, Move (x+w) (y+h), Move x (y+h), Move x y]
--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
facegen :: Int -> Int -> Prog
facegen x y = sizableBox x y 5 5 ++ sizableBox (x+1) (y+1) 3 1 ++ sizableBox (x+1) (y+3) 1 1 ++ sizableBox (x+3) (y+3) 1 1

-- randface :: Int -> Prog
-- randface 0 = []
-- randface n = facegen (randomRIO (1, 10)) (randomRIO (1, 10)) ++ randface (n-1)

amazing :: Prog
amazing = sizableBox 8 0 16 12 ++ sizableBox 12 12 10 10 ++ facegen 14 22
