{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import Prelude hiding ((==),(<), (-), (+), (*), (/), (&&), (!), (>>), print)
import EDSL
import qualified Interpreter as I
import qualified VM
import qualified Compiler as C

-- Examples

-- We need the type annotatons because 1 could be any number (Int, Float, ...)
ex1 = (1 :: Int) == (2 :: Int)


ex2 = True && ex1


-- We make use of parentheses that are not explicitly available in Exp.
ex3 = ((1 :: Int) + (2 :: Int)) * (3 :: Int)


x = var int "x"

ex5 = (1 :: Int) < x



cmd1 = let x = var int "x"
       in new x (1 :: Int) (print x)

cmd1b = let x = var int "x"
        in new x (1 :: Int) $
              print x
              >>
              x =:= x - (1::Int)
              >>
              print x

cmd2 = let x = var int "x"
       in new x (1 :: Int) $
             print x
             >>
             ifThenElse (x < (2 :: Int))
                        (print x)
                        skip


cmd2b = let x = var int "x"
        in new x (1 :: Int)
            (while (x < (10 :: Int))
                   skip)


cmd3 = let x = var int "x"
       in new x (1 :: Int)
            (while (x < (10 :: Int)) $
                   x =:= x + (1::Int)
                   >>
                   print x)


-- | Some derived EDSL commands.
-- print only accepts variables as arguments.
-- We introduce two further primitives that allow to print Integer/Boolean expressions.
printInt v = let x = var int "x"
             in new x v $ print x

printBool v = let x = var int "x"
              in new x v $ print x



evall = I.run

compileAndExecute cmd = do let code = C.compile cmd
                           VM.run code


-- Further design issues
-- A type error manifests itself as an unresolved instance


{-

-- Yields
-- No instance for (EXPOrd Int Bool c0) arising from a use of ‘==’
illTypedExpression = (1 :: Int) == True

-}


-- Instead of reporting IMP type errors as unresolved type class errors,
-- we could resolve such errors by providing additional instances.

instance EXPOrd Bool Int (ExpTy Bool) where
  (==) _ _ = error "Type error == \n mismatch between left operand (Bool) and right operand (Int)"
  (<) _ _ = error "Type error < \n mismatch between left operand (Bool) and right operand (Int)"


illTypedExpression2 = True == (1 :: Int)

{-

Showing the above expression yields

*** Exception: Type error ==
 mismatch between left operand (Bool) and right operand (Int)

-}