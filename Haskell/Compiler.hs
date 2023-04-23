-- Compile to VM code

module Compiler where

import Control.Monad
import Control.Monad.State

import qualified Syntax as S
import VM


compile :: S.Cmd -> Code
compile cmd =
  let (cmdFresh,_) = runState (freshNewVar cmd) 1
      (env,_) = runState (memLoc cmdFresh) 1
      (code,_) = runState (compileCmd env cmdFresh) World{lbl = 1}
  in code

-- | Rename variables introduced via Newvar such that
-- their names is fresh
freshNewVar :: S.Cmd -> State Int S.Cmd
freshNewVar (S.Newvar v e c) = do
   i <- get
   put (i+1)
   let new = "freshVarName" ++ show i
   let old = v
   let renameCmd = replaceCmd (new,old) c
   cmd' <- freshNewVar renameCmd
   return $ S.Newvar new e cmd'
freshNewVar S.Skip = return S.Skip
freshNewVar (a@S.Assign{}) = return a
freshNewVar (S.Seq c1 c2) = do
   c1' <- freshNewVar c1
   c2' <- freshNewVar c2
   return (S.Seq c1' c2')
freshNewVar (S.ITE e c1 c2) = do
   c1' <- freshNewVar c1
   c2' <- freshNewVar c2
   return (S.ITE e c1' c2')
freshNewVar (S.While e c) = do
   c' <- freshNewVar c
   return (S.While e c')
freshNewVar (p@S.Print{}) = return p

replaceExp :: (S.Var,S.Var) -> S.Exp -> S.Exp
replaceExp _ (v@S.Val{}) = v
replaceExp (new,old) (S.Id v)
   | v == old  = S.Id new
   | otherwise = S.Id v
replaceExp no (S.Plus e1 e2) =
  S.Plus (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.Minus e1 e2) =
  S.Minus (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.Times e1 e2) =
  S.Times (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.Div e1 e2) =
  S.Div (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.Equal e1 e2) =
  S.Equal (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.LessThan e1 e2) =
  S.LessThan (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.And e1 e2) =
  S.And (replaceExp no e1) (replaceExp no e2)
replaceExp no (S.Not e) =
  S.Not (replaceExp no e)

replaceCmd :: (S.Var,S.Var) -> S.Cmd -> S.Cmd
replaceCmd (new,old) (S.Newvar v e c)
  | v == old  = S.Newvar v (replaceExp (new,old) e) c
  | otherwise = S.Newvar v (replaceExp (new,old) e) (replaceCmd (new,old) c)
replaceCmd _ S.Skip = S.Skip
replaceCmd (new,old) (S.Assign v e)
  | v == old  = S.Assign new (replaceExp (new,old) e)
  | otherwise = S.Assign v (replaceExp (new,old) e)
replaceCmd no (S.Seq c1 c2) =
  S.Seq (replaceCmd no c1) (replaceCmd no c2)
replaceCmd no (S.ITE e c1 c2) =
  S.ITE (replaceExp no e) (replaceCmd no c1) (replaceCmd no c2)
replaceCmd no (S.While e c) =
  S.While (replaceExp no e) (replaceCmd no c)
replaceCmd (new, old) (S.Print v)
  | v == old  = S.Print new
  | otherwise = S.Print v


-- | Mapping of variables to memory locations
type Env = [(String,Int)]

applyEnv env s =
  case (lookup s env) of
    Just l -> l
    Nothing -> error "location for variable not found"


-- | Assign memory locations to variables
memLoc :: S.Cmd -> State Int Env
memLoc S.Skip = return []
memLoc (S.Assign{}) = return []
memLoc (S.Seq c1 c2) = do
    m1 <- memLoc c1
    m2 <- memLoc c2
    return $ m1 ++ m2
memLoc (S.ITE _ c1 c2) = do
    m1 <- memLoc c1
    m2 <- memLoc c2
    return $ m1 ++ m2
memLoc (S.While _ c) = memLoc c
memLoc (S.Newvar v _ c) = do
  i <- get
  put $ i+1
  m <- memLoc c
  return $ (v,i) : m
memLoc (S.Print{}) = return []

-- | Compile expressions.
-- Invariant: Result (evaluated expression) will be on top of stack
compileExp :: Env -> S.Exp -> State World Code
compileExp _ (S.Val (S.N i)) = return [Push i]
compileExp _ (S.Val S.BTrue) = return [Push 1]
compileExp _ (S.Val S.BFalse) = return [Push 0]
compileExp env (S.Id v) = return [Dref $ applyEnv env v]
compileExp env (S.Plus e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   return $ c2 ++ c1 ++ [Add]
      -- first push e2 then e1
compileExp env (S.Minus e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   return $ c2 ++ c1 ++ [Minus]
compileExp env (S.Times e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   return $ c2 ++ c1 ++ [Mult]
compileExp env (S.Div e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   return $ c2 ++ c1 ++ [Div]
compileExp env (S.Not e) = do
   c <- compileExp env e
   setTrue <- freshLabel
   continue <- freshLabel
   return $ c
            ++ [Zero setTrue]
            ++ [Push 0, Jump continue]
            ++ [Lbl setTrue, Push 1]
            ++ [Lbl continue]
compileExp env (S.And e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   setFalse <- freshLabel
   continue <- freshLabel
   return $ c1
            ++ [Zero setFalse]
            ++ c2
            ++ [Zero setFalse]
            ++ [Push 1, Jump continue]
            ++ [Lbl setFalse, Push 0]
            ++ [Lbl continue]
compileExp env (S.Equal e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   return $ c2 ++ c1 ++ [Eq]
compileExp env (S.LessThan e1 e2) = do
   c1 <- compileExp env e1
   c2 <- compileExp env e2
   return $ c2 ++ c1 ++ [Lt]

-- Some global state we thread through during compilation of commands.
-- Currently, we only require a label supply
data World = World { lbl :: Label }

freshLabel :: State World Label
freshLabel = do
  s <- get
  let l = lbl s
  put s { lbl = l + 1 }
  return l

-- | Compile commands.
-- Assumes that all new variables are fresh and thus we can
-- map NewVar to Assign
compileCmd :: Env -> S.Cmd -> State World Code
compileCmd env (S.Newvar v e c) =
   compileCmd env (S.Seq (S.Assign v e) c)
compileCmd _ S.Skip = return []
compileCmd env (S.Assign v e) = do
    c <- compileExp env e
    return $ c ++ [Asg $ applyEnv env v]
compileCmd env (S.Seq c1 c2) = do
   code1 <- compileCmd env c1
   code2 <- compileCmd env c2
   return $ code1 ++ code2
compileCmd env (S.ITE e c1 c2) = do
   codeE <- compileExp env e
   codeThen <- compileCmd env c1
   codeElse <- compileCmd env c2
   labelElse <- freshLabel
   labelEndITE <- freshLabel
   return $    codeE
            ++ [Zero labelElse]
            ++ codeThen
            ++ [Jump labelEndITE]
            ++ [Lbl labelElse]
            ++ codeElse
            ++ [Lbl labelEndITE]
compileCmd env (S.While e c) = do
   codeE <- compileExp env e
   codeBody <- compileCmd env c
   startWhile <- freshLabel
   endWhile <- freshLabel
   return $    [Lbl startWhile]
            ++ codeE
            ++ [Zero endWhile]
            ++ codeBody
            ++ [Jump startWhile]
            ++ [Lbl endWhile]
compileCmd env (S.Print v) =
   return $ [Output $ applyEnv env v]