-- Names:
--  Rikki Gibson -- ONID: gibsonri
--  Benjamin Narin -- ONID: narinb
module MiniLogo where

import Prelude hiding(Num)
import Data.List

---------- Task 1 -----------
type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]



data Mode = Down | Up
  deriving (Eq, Show)

data Expr = Var Var
          | Num Num
          | Add Expr Expr
  deriving (Eq, Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq, Show)

---------- Task 2 -----------
-- define line(x1, y1, x2, y2) {
--   pen up; move (x1, y1); pen down; move (x2, y2);
-- }
line :: Cmd
line = Define "line"
              ["x1", "y1", "x2", "y2"]
              [Pen Up, Move (Var "x1") (Var "y1"), Pen Down, Move (Var "x2") (Var "y2")]

---------- Task 3 -----------
-- define nix(x, y, w, h) {
--   call line(x, y, x + w, y + h);
--   call line(x + w, y, x, y + h);
-- }
nix :: Cmd
nix = Define "nix"
             ["x", "y", "w", "h"]
             [Call "line"
                [Var "x",
                 Var "y",
                 Add (Var "x") (Var "w"),
                 Add (Var "y") (Var "h")],
              Call "line"
                [Add (Var "x") (Var "w"),
                 Var "y",
                 Var "x",
                 Add (Var "y") (Var "h")]]

---------- Task 4 -----------
-- |
--   >>> steps 0
--   []
--   >>> steps 1
--   [Pen Up,Move (Num 0) (Num 0),Pen Down,Move (Num 0) (Num 1),Move (Num 1) (Num 1)]
--   >>> steps 2
--   [Pen Up,Move (Num 0) (Num 0),Pen Down,Move (Num 0) (Num 1),Move (Num 1) (Num 1),Move (Num 1) (Num 2),Move (Num 2) (Num 2)]
steps :: Int -> Prog
steps 0 = []
steps n = Pen Up : Move (Num 0) (Num 0) : Pen Down : stepsAcc 0 0 n

-- | Takes a starting X,Y coordinate and a number of steps to draw.
stepsAcc :: Int -> Int -> Int -> Prog
stepsAcc _ _ 0 = []
stepsAcc x y n = [Move (Num x) (Num (y+1)), Move (Num (x+1)) (Num (y+1))]
                  ++ stepsAcc (x+1) (y+1) (n-1)

---------- Task 5 -----------
-- |
--   >>> macros [Pen Up,Define "foo" [] [],Define "bar" [] [],Pen Up,Pen Up]
--   ["foo","bar"]
macros :: Prog -> [Macro]
macros [] = []
macros (x:xs) = case x of
    (Define name _ _) -> [name]
    _ -> []
  ++ macros xs

---------- Task 6 -----------
-- |
pretty :: Prog -> String
pretty p = intercalate "\n" (map prettyCmd p)

prettyCmd :: Cmd -> String
prettyCmd (Pen p) = "pen " ++ prettyMode p ++ ";"
prettyCmd (Move x y) = "move (" ++ prettyExpr x ++ "," ++ prettyExpr y ++ ");"
prettyCmd (Define name vars prog) = "define " ++ name ++ " (" ++ intercalate "," vars ++ ") {\n"
  ++ intercalate "\n" (map prettyCmd prog) ++ "\n}\n"
prettyCmd (Call name exprs) = "call " ++ name ++ " (" ++ intercalate "," (map prettyExpr exprs) ++ ");"

prettyMode :: Mode -> String
prettyMode Up = "up"
prettyMode Down = "down"

prettyExpr :: Expr -> String
prettyExpr (Var v) = v
prettyExpr (Num n) = show n
prettyExpr (Add e1 e2) = prettyExpr e1 ++ "+" ++ prettyExpr e2

---------- Task 7 -----------
optE :: Expr -> Expr
optE (Add (Num n1) (Num n2)) = Num (n1 + n2)
optE e = e

---------- Task 8 -----------
-- |
--   >>> optP [Move (Add (Num 3) (Num 4)) (Num 5), Pen Up, Move (Var "x") (Num 4)]
--   [Move (Num 7) (Num 5),Pen Up,Move (Var "x") (Num 4)]
optP :: Prog -> Prog
optP = map optC

optC :: Cmd -> Cmd
optC (Move e1 e2) = Move (optE e1) (optE e2)
optC (Call m exprs) = Call m (map optE exprs)
optC cmd = cmd
