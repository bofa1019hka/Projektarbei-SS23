-- Abstract syntax of IMP

module Syntax where

    type Var = String
    data Values = N Int | BTrue | BFalse deriving Show

    data Exp = Val Values | Id Var | Plus Exp Exp | Minus Exp Exp | Times Exp Exp
         | Div Exp Exp | Equal Exp Exp | LessThan Exp Exp | And Exp Exp | Not Exp deriving Show

    data Cmd = Skip | Assign Var Exp | Seq Cmd Cmd | ITE Exp Cmd Cmd
         | While Exp Cmd | Newvar Var Exp Cmd | Print Var
         deriving Show