module SalsaInterp (Position, interpolate, runProg) where

import SalsaAst
import Gpx
import qualified Data.Map as M

--
-- The function interpolate
--

-- I want to avoid using Doubles as, meaning slightly less
-- precise calculations, but prettier code and more readable
type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate framerate (x1,y1) pe@(x2,y2) =
  let
    rate = 100 `div` framerate
    xdis = (x2 - x1) * rate
    ydis = (y2 - y1) * rate
  in
   [(x,y)| i <- [1..(framerate-1)], x <- [(x1+((xdis*i) `div` 100))],
          y <- [(y1+((ydis*i) `div` 100))]]++[pe]
    

--
-- Define the types Context and SalsaCommand
--


-- The enivronment must be read-only
-- The state must be read/write
data Context = Context Environment State
-- The environment bindings, list of active views, framerate 
type Environment = (M.Map Ident Definition, [Ident], Int)
-- The state
type State = String

createEmptyContext :: Int -> Context
createEmptyContext n = Context (M.empty, [], n) ""

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> Either String a}

instance Monad SalsaCommand where
  return k = SalsaCommand (\_ -> Right k)
-- "Note that a command cannot change the environment (part (1) of the context above),
-- your type should reflect this."
  (SalsaCommand a) >>= f = SalsaCommand (\c -> case a c of Right a1 -> runSC (f a1) c
                                                           Left a2 -> Left a2)

-- functions for manipulating the context



--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command = undefined
-- What must happen here?
-- 1. Must turn a Command into a grx instruction
-- 2. And return it

--
-- Define the type Salsa
--

-- http://www.haskell.org/tutorial/monads.html
data Salsa a = Salsa ((Context, Animation) -> (a,(Context,Animation)))

instance Monad Salsa where
  return k = Salsa (\(c,a) -> (k,(c,a)))
  (Salsa a) >>= f = Salsa (\d -> let (r,n) = a d
                                     (Salsa b) = f r
                                 in
                                  b n
                          )

--
-- Define the functions liftC, definition, and defCom
--

definition :: Definition -> Salsa ()
definition = undefined
-- What must happend here?
-- 1. The definition must be added to the context, ie create a binding


defCom :: DefCom -> Salsa ()
defCom = undefined
-- What must happend here?
-- 1. Must notice if it is a command or a definition
-- 2. Use the proper monad to intepret the input

liftC :: SalsaCommand a -> Salsa a
liftC = undefined
-- What must happend here?
-- I don't know

--
-- Define the function runProg
--

--ask :: Salsa (Context, Animation)
--ask = Salsa $ \(c,a) -> (c,a)

runProg :: Integer -> Program -> Animation
runProg framerate program = undefined
-- What must happend here?
-- 1. An empty/new context must be created
-- 2. Then each DefCom in Program must be run
---- Program is an alias for [DefCom]
---- A DefCom can either be a Definition of a Command
-- 3. Return the created Animation
