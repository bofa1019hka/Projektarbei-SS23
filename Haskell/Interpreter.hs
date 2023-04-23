-- Operatonal big-step semantics (interpreter)

module Interpreter where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Syntax


----------------------
-- state operations --
----------------------

-- | We don't check if

type VarState = [(Var, Values)]

lookupVar :: VarState -> Var -> Values
lookupVar s id = case (lookup id s) of
                    Just v -> v

-- | We simply add the new binding to the front.
addVarState :: VarState -> Var -> Values -> VarState
addVarState s id v = (id, v) : s


-- | Update binding.
updateVarState :: VarState -> Var -> Values -> VarState
updateVarState [] id v = []
updateVarState ((id',v'):s) id v
  | id' == id = (id,v) : s
  | otherwise = (id',v') : updateVarState s id v


-- | We remove the first binding of id.
-- Any later (in the last) bindings of id will be kept.
-- This is useful to in case of 'newvar' where we temporarily add a binding,
-- and then remove this binding again.
removeVarState :: VarState -> Var -> VarState
removeVarState [] _ = []
removeVarState ((id,v):s) id'
   | id == id' = s
   | otherwise = (id,v) : removeVarState s id'

--------------------------
-- expression semantics --
--------------------------


-- | Evaluation of expressions is pure
evalExp :: VarState -> Exp -> Values
evalExp _ (Val v) = v
evalExp s (Id id) = lookupVar s id
evalExp s (Plus e1 e2) = let (N i1) = evalExp s e1
                             (N i2) = evalExp s e2
                         in N (i1 + i2)
evalExp s (Minus e1 e2) = let (N i1) = evalExp s e1
                              (N i2) = evalExp s e2
                          in N (i1 - i2)
evalExp s (Times e1 e2) = let (N i1) = evalExp s e1
                              (N i2) = evalExp s e2
                          in N (i1 * i2)
evalExp s (Div e1 e2) = let (N i1) = evalExp s e1
                            (N i2) = evalExp s e2
                        in if i2 /= 0
                           then N (i1 * i2)
                           else undefined
evalExp s (Equal e1 e2) = let v1 = evalExp s e1
                              v2 = evalExp s e2
                              eqB BTrue BTrue = BTrue
                              eqB BFalse BFalse = BTrue
                              eqB _ _ = BFalse
                              eqInt i1 i2
                                | i1 == i2  = BTrue
                                | otherwise = BFalse
                          in case v1 of
                              N i1   -> case v2 of
                                         N   i2 -> eqInt i1 i2
                                         _      -> error "Equal: Incompatible operands"
                              _      -> case v2 of
                                         N _   -> error "Equal: Incompatible operands"
                                         _     -> eqB v1 v2
evalExp s (LessThan e1 e2) = let (N i1) = evalExp s e1
                                 (N i2) = evalExp s e2
                             in if i1 < i2
                                then BTrue
                                else BFalse

-----------------------
-- command semantics --
-----------------------

-- | We need IO because of print.
type EVAL a = StateT VarState IO a

evalCmd :: Cmd -> EVAL()
evalCmd Skip = return ()
evalCmd (Print id) = do s <- get
                        let x = lookupVar s id
                        let showValues (N i) = show i
                            showValues BTrue = "true"
                            showValues BFalse = "false"
                        liftIO $ putStrLn (showValues x)
                        return ()
evalCmd (Assign id e) = do s <- get
                           let s' = updateVarState s id (evalExp s e)
                           put s'
                           return ()
evalCmd (Seq c1 c2) = do evalCmd c1
                         evalCmd c2

evalCmd (ITE e c1 c2) = do s <- get
                           let v = evalExp s e
                           case v of
                             BTrue  -> evalCmd c1
                             BFalse -> evalCmd c2
                             _      -> error "ITE: invalid condition"
evalCmd (While e c) = do s <- get
                         let v = evalExp s e
                         case v of
                            BFalse -> return ()
                            BTrue  -> do evalCmd c
                                         evalCmd (While e c)
                            _      -> error "While: invalid condition"
evalCmd (Newvar id e c) = do s <- get
                             let s' = addVarState s id (evalExp s e)
                             put s'
                             evalCmd c
                             s'' <- get
                             put $ removeVarState s'' id


run :: Cmd -> IO VarState
run cmd = do (_,s) <- runStateT (evalCmd cmd) []
             return s