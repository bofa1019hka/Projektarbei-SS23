{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module EDSL where

import Prelude hiding ((==),(<), (-), (+), (*), (/), (&&), (!))

import Syntax

-- Helpers

mkInt :: Int -> Exp
mkInt i = Val (N i)

mkBool :: Bool -> Exp
mkBool True = Val BTrue
mkBool False = Val BFalse


-- Strongly typed interface
----------------------------------------

-- Phantom type

data ExpTy a = MkEx Exp deriving Show
unMk (MkEx e) = e

data VarTy a = MkVar Var
unVar (MkVar e) = e

mkIntTy :: Int -> ExpTy Int
mkIntTy i = MkEx $ Val $ N i

mkBoolTy :: Bool -> ExpTy Bool
mkBoolTy True = MkEx $ Val BTrue
mkBoolTy False = MkEx $ Val BFalse

-- variables

int :: Int
int = undefined

bool :: Bool
bool = undefined

varToE :: VarTy a -> ExpTy a
varToE (MkVar x) = MkEx (Id x)

class VAR a where
  var :: a -> String -> VarTy a

instance VAR Int where
  var _ x = MkVar x

instance VAR Bool where
  var _ x = MkVar x

-- expressions

-- infixr 6 -, +
infixr 7 *,/
class EXPInt a b c | a b -> c where
  (-) :: a -> b -> c
  (+) :: a -> b -> c
  (*) :: a -> b -> c
  (/) :: a -> b -> c

class EXPBool a b c | a b -> c where
  (&&) :: a -> b -> c

class EXPOrd a b c | a b -> c where
  (==) :: a -> b -> c
  (<) :: a -> b -> c

class NOT a b | a -> b where
  (!) :: a -> b

-- commands

infix 5 =:=
(=:=) :: VarTy a -> ExpTy a -> Cmd
(=:=) (MkVar x)  e = Assign x (unMk e)

infixr 4 >>
(>>) :: Cmd -> Cmd -> Cmd
(>>) c1 c2 = Seq c1 c2

while :: ExpTy Bool -> Cmd -> Cmd
while e c = While (unMk e) c

print :: (VarTy a) -> Cmd
print (MkVar x) = Print x

ifThenElse :: ExpTy Bool -> Cmd -> Cmd -> Cmd
ifThenElse e c1 c2 = ITE (unMk e) c1 c2

skip :: Cmd
skip = Skip

class NEW a b where
  new :: a -> b -> Cmd -> Cmd

instance NEW (VarTy a) (ExpTy a) where
  new (MkVar x) e c = Newvar x (unMk e) c

instance NEW (VarTy Int) Int where
  new (MkVar x) i c = Newvar x (mkInt i) c

instance NEW (VarTy Bool) Bool where
  new (MkVar x) b c = Newvar x (mkBool b) c



-- Instances
------------------

-- Negation

instance NOT Bool (ExpTy Bool) where
  (!) b = MkEx $ Not $ mkBool b

instance NOT (ExpTy Bool) (ExpTy Bool) where
  (!) e = MkEx $ Not $ unMk e

-- Integer expressions
-- Consider all combinations of (ExpTy Int), (VarTy Int) and Int.

instance EXPInt (ExpTy Int) (ExpTy Int) (ExpTy Int) where
  (-) e1 e2  = MkEx $ Minus (unMk e1) (unMk e2)
  (+) e1 e2  = MkEx $ Plus (unMk e1) (unMk e2)
  (*) e1 e2  = MkEx $ Times (unMk e1) (unMk e2)
  (/) e1 e2  = MkEx $ Div (unMk e1) (unMk e2)

instance EXPInt (ExpTy Int) (ExpTy Int) (ExpTy Int) => EXPInt Int (ExpTy Int) (ExpTy Int) where
  (-) i e  = (-) (mkIntTy i) e
  (+) i e  = (+) (mkIntTy i) e
  (*) i e  = (*) (mkIntTy i) e
  (/) i e  = (/) (mkIntTy i) e

instance EXPInt Int (ExpTy Int) (ExpTy Int) => EXPInt (ExpTy Int) Int (ExpTy Int) where
  (-) e i  = (-) i e
  (+) e i  = (+) i e
  (*) e i  = (*) i e
  (/) e i  = (/) i e

-- We map (Int, Int) to (ExpTy Int, ExpTy Int).
-- We could also map (Int, Int) to (ExpTy Int, Int) which would save some code in the method definitions.
instance EXPInt (ExpTy Int) (ExpTy Int) (ExpTy Int) => EXPInt Int Int (ExpTy Int) where
  (-) i1 i2 = (-) (mkIntTy i1) (mkIntTy i2)
  (+) i1 i2 = (+) (mkIntTy i1) (mkIntTy i2)
  (*) i1 i2 = (*) (mkIntTy i1) (mkIntTy i2)
  (/) i1 i2 = (/) (mkIntTy i1) (mkIntTy i2)

instance EXPInt (ExpTy Int) (ExpTy Int) (ExpTy Int) => EXPInt (VarTy Int) (ExpTy Int) (ExpTy Int) where
  (-) v e  = (-) (varToE v) e
  (+) v e  = (+) (varToE v) e
  (*) v e  = (*) (varToE v) e
  (/) v e  = (/) (varToE v) e

instance EXPInt (VarTy Int) (ExpTy Int) (ExpTy Int) => EXPInt (ExpTy Int) (VarTy Int) (ExpTy Int) where
  (-) e v = (-) v e
  (+) e v = (+) v e
  (*) e v = (*) v e
  (/) e v = (/) v e

instance EXPInt (VarTy Int) (ExpTy Int) (ExpTy Int) => EXPInt (VarTy Int) (VarTy Int) (ExpTy Int) where
  (-) v1 v2 = (-) v1 (varToE v2)
  (+) v1 v2 = (+) v1 (varToE v2)
  (*) v1 v2 = (*) v1 (varToE v2)
  (/) v1 v2 = (/) v1 (varToE v2)

instance EXPInt (ExpTy Int) Int (ExpTy Int) => EXPInt (VarTy Int) Int (ExpTy Int) where
  (-) v i = (-) (varToE v) i
  (+) v i = (+) (varToE v) i
  (*) v i = (*) (varToE v) i
  (/) v i = (/) (varToE v) i

instance EXPInt (VarTy Int) Int (ExpTy Int) => EXPInt Int (VarTy Int) (ExpTy Int) where
  (-) i v = (-) v i
  (+) i v = (+) v i
  (*) i v = (*) v i
  (/) i v = (/) v i

-- Boolean expressions:
-- Consider all combinations of (ExpTy Bool), (VarTy Bool) and Bool.

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) where
  (&&) e1 e2 = MkEx $ And (unMk e1) (unMk e2)

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPBool Bool (ExpTy Bool) (ExpTy Bool) where
  (&&) b e = (&&) (mkBoolTy b) e

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPBool (ExpTy Bool) Bool (ExpTy Bool) where
  (&&) e b = (&&) e (mkBoolTy b)

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPBool Bool Bool (ExpTy Bool) where
  (&&) b1 b2 = (&&) (mkBoolTy b1) (mkBoolTy b2)

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPBool (VarTy Bool) (ExpTy Bool) (ExpTy Bool) where
  (&&) v b = (&&) (varToE v) b

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPBool (ExpTy Bool) (VarTy Bool) (ExpTy Bool) where
  (&&) b v = (&&) b (varToE v)

instance EXPBool (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPBool (VarTy Bool) (VarTy Bool) (ExpTy Bool) where
  (&&) v1 v2 = (&&) (varToE v1) (varToE v2)

instance EXPBool (ExpTy Bool) Bool (ExpTy Bool) => EXPBool (VarTy Bool) Bool (ExpTy Bool) where
  (&&) v b = (&&) (varToE v) b

instance EXPBool Bool (ExpTy Bool) (ExpTy Bool) => EXPBool Bool (VarTy Bool) (ExpTy Bool) where
  (&&) b v = (&&) b (varToE v)

-- Ordering expressions
-- Consider all combinations.

-- int cases
instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) where
  (==) e1 e2 = MkEx $ Equal (unMk e1) (unMk e2)
  (<) e1 e2 = MkEx $ LessThan (unMk e1) (unMk e2)

instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) => EXPOrd Int (ExpTy Int) (ExpTy Bool) where
  (==) i e = (==) (mkIntTy i) e
  (<) i e = (<) (mkIntTy i) e

instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) => EXPOrd (ExpTy Int) Int (ExpTy Bool) where
  (==) e i = (==) e (mkIntTy i)
  (<) e i = (<) e (mkIntTy i)

instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) => EXPOrd Int Int (ExpTy Bool) where
  (==) i1 i2 = (==) (mkIntTy i1) (mkIntTy i2)
  (<) i1 i2 = (<) (mkIntTy i1) (mkIntTy i2)

instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) => EXPOrd (VarTy Int) (ExpTy Int) (ExpTy Bool) where
  (==) v e = (==) (varToE v) e
  (<) v e = (<) (varToE v) e

instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) => EXPOrd (ExpTy Int) (VarTy Int) (ExpTy Bool) where
  (==) e v = (==) e (varToE v)
  (<) e v = (<) e (varToE v)

instance EXPOrd (ExpTy Int) (ExpTy Int) (ExpTy Bool) => EXPOrd (VarTy Int) (VarTy Int) (ExpTy Bool) where
  (==) v1 v2 = (==) (varToE v1) (varToE v2)
  (<) v1 v2 = (<) (varToE v1) (varToE v2)

instance EXPOrd (ExpTy Int) Int (ExpTy Bool) => EXPOrd (VarTy Int) Int (ExpTy Bool) where
  (==) v i = (==) (varToE v) i
  (<) v i = (<) (varToE v) i

instance EXPOrd Int (ExpTy Int) (ExpTy Bool) => EXPOrd Int (VarTy Int) (ExpTy Bool) where
  (==) i v = (==) i (varToE v)
  (<) i v = (<) i (varToE v)

-- bool cases
instance EXPOrd (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) where
  (==) e1 e2 = MkEx $ Equal (unMk e1) (unMk e2)
  (<) e1 e2 = MkEx $ LessThan (unMk e1) (unMk e2)

instance EXPOrd (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPOrd Bool (ExpTy Bool) (ExpTy Bool) where
  (==) b e = (==) (mkBoolTy b) e
  (<) b e = (<) (mkBoolTy b) e

instance EXPOrd (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPOrd (ExpTy Bool) Bool (ExpTy Bool) where
  (==) e b = (==) e (mkBoolTy b)
  (<) e b = (<) e (mkBoolTy b)

instance EXPOrd (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPOrd Bool Bool (ExpTy Bool) where
  (==) b1 b2 = (==) (mkBoolTy b1) (mkBoolTy b2)
  (<) b1 b2 = (<) (mkBoolTy b1) (mkBoolTy b2)

instance EXPOrd (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPOrd (VarTy Bool) (ExpTy Bool) (ExpTy Bool) where
  (==) v e = (==) (varToE v) e
  (<) v e = (<) (varToE v) e

-- Just for fun we map the case (ExpTy Bool, VarTyBool) to (VarTyBool, ExpTyBool).
instance EXPOrd (VarTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPOrd (ExpTy Bool) (VarTy Bool) (ExpTy Bool) where
  (==) e v = (==) v e
  (<) e v = (<) v e

instance EXPOrd (ExpTy Bool) (ExpTy Bool) (ExpTy Bool) => EXPOrd (VarTy Bool) (VarTy Bool) (ExpTy Bool) where
  (==) v1 v2 = (==) (varToE v1) (varToE v2)
  (<) v1 v2 = (<) (varToE v1) (varToE v2)

instance EXPOrd (ExpTy Bool) Bool (ExpTy Bool) => EXPOrd (VarTy Bool) Bool (ExpTy Bool) where
  (==) v b = (==) (varToE v) b
  (<) v b = (<) (varToE v) b

-- Here we map (Bool, VarTy Bool) to (Bool, ExpTy Bool).
instance EXPOrd Bool (ExpTy Bool) (ExpTy Bool) => EXPOrd Bool (VarTy Bool) (ExpTy Bool) where
  (==) b v = (==) b (varToE v)
  (<) b v = (<) b (varToE v)