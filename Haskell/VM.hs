-- A simple stack-based virtual machine

module VM where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

-- linear address space
-- variables stored on (pre-allocated) heap
-- values are all Ints
-- represent true by 1 and false by 0

type Label = Int
type Location = Int

-- Memory starts at location 0 !!!
type Mem = [Int]

-- computations performed on stack
type Stack = [Int]

-- VM instructions

-- Add, Minus, Lt, Gt, Div, Mult, Eq
-- operands are poped from stack (op1 top-most element),
-- result (op1 op op 2) is pushed onto stack

-- Push value
-- push value onto stack

-- Lbl l
-- introduces label l, skip statement

-- NonZero l, Zero l
-- jump to location l if poped value is (non)zero

-- Jump l
-- jump to location l

-- Dref l
-- lookup value at location l, push onto stack

-- Asg l
-- assign poped value to location l

data Instruction = Add | Minus | Lt | Gt | Div | Mult | Eq
                 | Push Int
                 | Lbl Label | NonZero Label | Zero Label
                 | Jump Label
                 | Dref Location | Asg Location
                 | Output Location deriving Show


type Code = [Instruction]


type VMState = (Stack, Mem)

type VM a = StateT VMState IO a

-- stack operations

pop :: Stack -> (Int,Stack)
pop [] = error "Empty stack"
pop (x:xs) = (x,xs)

push :: Stack -> Int -> Stack
push xs x = x:xs

top :: Stack -> Int
top [] = error "Empty stack"
top (x:xs) = x

popS :: VM Int
popS = do (s,m) <- get
          let (v,s') = pop s
          put (s',m)
          return v

pushS :: Int -> VM ()
pushS v = do (s,m) <- get
             put (push s v, m)

topS :: VM Int
topS = do (s,_) <- get
          return $ top s


-- memory operations

getValue :: Location -> VM Int
getValue l = do (_,m) <- get
                return (m !! l)

setValue :: Location -> Int -> VM ()
setValue l v = let update (x:xs) 0 v = v:xs
                   update (x:xs) l v = x:(update xs (l-1) v)
                   -- Option: employ exception monad for better error handling
                   update _ _ _      = error "invalid memory location access"
               in do (s,m) <- get
                     put (s, update m l v)


---- Interpreter -----

-- Booleans are represented by Ints
coerce :: Bool -> Int
coerce True = 1
coerce False = 0

-- jump to location l
locate :: Label -> Code -> Code
locate l [] = error "Label not found"
locate l (Lbl l':next)
  | l == l'    = next
  | otherwise  = locate l next
locate l (_:next) = locate l next




-- interpreter pc c runs program c with program counter pc
interp :: Code -> Code -> VM ()
interp [] p = return ()
-- operands are poped from stack, result is pushed onto stack
interp (Add:next) p = do op1 <- popS
                         op2 <- popS
                         pushS (op1 + op2)
                         interp next p
interp (Minus:next) p = do op1 <- popS
                           op2 <- popS
                           pushS (op1 - op2)
                           interp next p
interp (Lt:next) p = do op1 <- popS
                        op2 <- popS
                        pushS (coerce (op1 < op2))
                        interp next p
interp (Gt:next) p = do op1 <- popS
                        op2 <- popS
                        pushS (coerce (op1 > op2))
                        interp next p
interp (Div:next) p = do op1 <- popS
                         op2 <- popS
                         if op2 == 0 then error "divison by zero"
                          else do pushS (op1 `div` op2)
                                  interp next p
interp (Mult:next) p = do op1 <- popS
                          op2 <- popS
                          pushS (op1 * op2)
                          interp next p
interp (Eq:next) p = do op1 <- popS
                        op2 <- popS
                        pushS (coerce (op1 == op2))
                        interp next p
-- push value onto stack
interp ((Push v):next) p = do pushS v
                              interp next p
-- introduces label l, skip statement
interp ((Lbl l):next) p = interp next p
-- jump if poped value is non zero
interp ((NonZero l):next) p = do v <- popS
                                 if not (v == 0) then interp ((Jump l):next) p
                                  else interp next p
-- jump if poped value is zero
interp ((Zero l):next) p = do v <- popS
                              if v == 0 then interp ((Jump l):next) p
                               else interp next p
-- jump to l
interp ((Jump l):next) p = interp (locate l p) p
-- get value at location l, push onto stack
interp ((Dref l):next) p = do v <- getValue l
                              pushS v
                              interp next p
-- assign poped value to location l
interp ((Asg l):next) p = do v <- popS
                             setValue l v
                             interp next p

-- print location
interp ((Output l):next) p = do n <- getValue l
                                liftIO $ putStrLn $ show n
                                interp next p

run :: Code -> IO Stack
run code = let mem :: [Int]
               mem = 0:mem -- memory infinite, initially zero!
               st = []

           in do (_,(st',_)) <- runStateT (interp code code) (st,mem)
                 return st'