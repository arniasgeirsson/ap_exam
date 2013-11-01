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
   [(x,y)| i <- [1..(framerate-1)], x <- [x1+((xdis*i) `div` 100)],
          y <- [y1+((ydis*i) `div` 100)]]++[pe]
    

--
-- Define the types Context and SalsaCommand
--


-- The enivronment must be read-only
-- The state must be read/write
data Context = Context ConEnvironment State
             deriving (Show)
-- The environment bindings, list of active views, framerate 
type ConEnvironment = (Environment, [Ident], Integer)
type Environment = M.Map Ident Definition
-- The state; (shape_id, view_id) position::(int,int)
type State = M.Map (Ident, Ident) Position

createEmptyContext :: Integer -> Context
createEmptyContext n = Context (M.empty, [], n) M.empty

-- newtype SalsaCommand a = SalsaCommand {runSC :: Context -> Either String a}

-- instance Monad SalsaCommand where
--   return k = SalsaCommand (\_ -> Right k)
-- -- "Note that a command cannot change the environment (part (1) of the context above),
-- -- your type should reflect this."
--   (SalsaCommand a) >>= f = SalsaCommand (\c -> case a c of Right a1 -> runSC (f a1) c
--                                                            Left a2 -> Left a2)



--
-- Define the function command
--
  
-- What must happen here?
-- 1. Must turn a Command into a grx instruction
-- 2. And return it
command :: Command -> Salsa ()
command (Move ids (Abs expx expy)) = do
  (con,anim) <- ask
  x <- evalExpr expx
  y <- evalExpr expy
  let (Context (env,active,fr) state) = con
      [all_instr] = map (\view -> map (\id ->
                                        let
                                          def = lookupIdent id env
                                        in
                                         interpolateShape def (x,y) fr view
                                      ) ids) active
                  
  updateSalsa (addInstructionsToCurrentFrame (concat all_instr))

interpolateShape :: Definition -> Position -> Integer -> ViewName-> [GpxInstr]
interpolateShape (Rectangle id (Const x1) (Const y1) (Const w) (Const h) col) endpos fr view =
  let
    allPos = interpolate fr (x1, y1) endpos
  in
   map (\(x,y) -> (DrawRect x y w h view (evalColour col))) allPos


addInstructionsToCurrentFrame :: [GpxInstr] -> (Context,Animation) -> (Context,Animation)
addInstructionsToCurrentFrame instr (con,anim) =
  let (views, frames) = anim
      (rest,[curframe]) = getLast frames
   in
   (con,(views,rest++[(curframe++instr)]))

getCurrentDefPos :: Definition -> Position
getCurrentDefPos (Rectangle _ (Const x) (Const y) _ _ _) = (x,y)

-- askCmd :: SalsaCommand Context
-- askCmd = SalsaCommand $ \con -> Right con

--
-- Define the type Salsa
--

data Salsa a = Salsa ((Context,Animation) -> (a,(Context,Animation)))

instance Monad Salsa where
  return k = Salsa $ \state -> (k,state)
  (Salsa a1) >>= f = Salsa $ \state0 -> let (r,state1) = a1 state0
                                            (Salsa a2) = f r
                                        in
                                         a2 state1

updateSalsa :: ((Context,Animation) -> (Context,Animation)) -> Salsa ()
updateSalsa f = Salsa $ \state -> ((), f state)

-- functions for manipulating the context

lookupIdent :: Ident -> Environment -> Definition
lookupIdent id env = case M.lookup id env
                     of Just def -> def
                        Nothing -> error ("Tried to look up unkown ident: "++id)

bindDefinition :: Ident -> Definition -> Environment -> Environment
bindDefinition = M.insert

addToContext :: (Environment -> Environment) -> (Context,Animation) -> (Context,Animation)
addToContext f (con,anim) =
  let (Context (env,active,fr) state) = con
      env' = f env
  in
   ((Context (env',active,fr) state),anim)

updateActiveViews :: [Ident] -> (Context,Animation) -> (Context,Animation)
updateActiveViews views (con,anim) =
  let (Context (env,active,fr) state) = con
  in
   ((Context (env,views,fr) state),anim)

placeShapeInActiveViews :: Definition -> (Context,Animation) -> (Context,Animation)
placeShapeInActiveViews (Rectangle id (Const x) (Const y) w h colour) (con,anim) =
  let (Context (env,active,fr) state) = con
      newState = foldl insert state active
      insert cur_state view_id = M.insert (id,view_id) (x,y) cur_state
  in
   ((Context (env,active,fr) newState),anim)

addViewToAnimation :: ViewName -> Integer -> Integer -> (Context,Animation) -> (Context,Animation)
addViewToAnimation id x y (con,anim) =
  let (views, frames) = anim
  in
   (con,(views++[(id,x,y)],frames))

placeShapeInCurrentFrame :: Definition -> (Context,Animation) -> (Context,Animation)
placeShapeInCurrentFrame (Rectangle id (Const x) (Const y) (Const w) (Const h) colour) (con,anim) =
  let (views, frames) = anim
      (Context (_,active,_) _) = con
      col = evalColour colour
      (rest,[curframe]) = getLast frames
      new_instr = map f active
      f view = (DrawRect x y w h view col)
  in
   (con,(views,rest++[(curframe++new_instr)]))

goToNextFrame :: (Context,Animation) -> (Context,Animation)
-- TODO set the init draws of all the defined shapes
goToNextFrame (con,(views, frames)) = (con,(views,(reverse ([]:(reverse frames)))))

getLast :: [a] -> ([a],[a])
getLast [] = ([],[])
getLast (x:[]) = ([],[x])
getLast (x:xs) = let (rest,last) = getLast xs
                 in
                  (x:rest,last)
--
-- Define the functions liftC, definition, and defCom
--
  
-- What must happend here?
-- 1. The definition must be added to the context, ie create a binding
definition :: Definition -> Salsa ()
definition (Viewdef id x y) = do
  valx <- evalExpr x
  valy <- evalExpr y
  updateSalsa (addToContext (bindDefinition id $ Viewdef id (Const valx) (Const valy)))
  updateSalsa (updateActiveViews [id])
  updateSalsa (addViewToAnimation id valx valy)
  
definition (Rectangle id x y w h colour) = do
  valx <- evalExpr x
  valy <- evalExpr y
  valw <- evalExpr w
  valh <- evalExpr h
  let newRect = Rectangle id (Const valx) (Const valy) (Const valw) (Const valh) colour
  updateSalsa (addToContext (bindDefinition id newRect))
  updateSalsa (placeShapeInActiveViews newRect)
  updateSalsa (placeShapeInCurrentFrame newRect)

evalExpr (Const int) = return int

evalColour Green = "green"

-- What must happend here?
-- 1. Must notice if it is a command or a definition
-- 2. Use the proper monad to intepret the input
defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = do
  command com
  updateSalsa goToNextFrame


-- What must happend here?
-- I don't know
-- liftC :: SalsaCommand a -> Salsa a
-- liftC m = undefined
  

--
-- Define the function runProg
--

-- read the state/context
ask :: Salsa (Context,Animation)
ask = Salsa $ \state -> (state,state)

runSalsa :: (Context,Animation) -> Salsa a -> (a,(Context,Animation))
runSalsa state (Salsa m) = m state

-- What must happend here?
-- 1. An empty/new context must be created
-- 2. Then each DefCom in Program must be run
---- Program is an alias for [DefCom]
---- A DefCom can either be a Definition of a Command
-- 3. Return the created Animation
-- runProg :: Integer -> Program -> Either String Context -- Animation
runProg framerate program =
  let (a1,(con,anim)) = runSalsa ((createEmptyContext (fromIntegral framerate)),([],[[]]))
                        (join (Salsa $ \con -> (mapM defCom program, con)))
      join mm = do
        m <- mm
        m
  in
   anim

testv = [ Def (Viewdef "Default" (Const 400) (Const 400))]
testv2 = [ Def (Viewdef "Default" (Const 400) (Const 400)),Def (Viewdef "Det" (Const 40) (Const 4))]


testProg0 = [ Def (Viewdef "Default" (Const 400) (Const 400))
            , Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green)]

testProg1 = [ Def (Viewdef "Default" (Const 400) (Const 400))
            , Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green)
            , Com (Move ["box"] (Abs (Const 10) (Const 200)))]

testProg2 = [ Def (Viewdef "Default" (Const 400) (Const 400))
            , Def (Rectangle "box" (Const 10) (Const 400) (Const 20) (Const 20) Green)
            , Com (Move ["box"] (Abs (Const 10) (Const 200)))
            , Com (Move ["box"] (Rel (Const 100) (Const 0)))
            , Com (Move ["box"] (Abs (Const 110) (Const 400)))
            , Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0)))]

testAnim0 = ([("Default", 400, 400)], [[(DrawRect 10 400 20 20 "Default" "green")]])
