----------------------------------------------------------------------
--- Student name: Arni Asgeirsson
--- Student KU-id: lwf986
----------------------------------------------------------------------
module SalsaInterp
       (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import qualified Data.Map as M
import qualified Data.Maybe as Mb
import qualified Control.Monad as Mo

------------------------------------------------------------
--------------------- The interface ------------------------
------------------------------------------------------------

type Position = (Integer, Integer)

interpolate :: Integer -> Position -> Position -> [Position]
interpolate 0 _ _ = []
interpolate framerate (x1,y1) pe@(x2,y2) =
  let
    rate = 100 `div` framerate
    xdis = (x2 - x1) * rate
    ydis = (y2 - y1) * rate
  in
   [(x,y)| i <- [1..(framerate-1)], x <- [x1+((xdis*i) `div` 100)],
          y <- [y1+((ydis*i) `div` 100)]]++[pe]

runProg :: Integer -> Program -> Animation
runProg n p =
  let (_,(_,anim)) = runSalsa (createEmptyContext n)
                   (Mo.join (Salsa $ \con -> (mapM defCom p, con)))
  in
   anim

------------------------------------------------------------
------------------------ Context ---------------------------
------------------------------------------------------------

--------------------- Data Structure -----------------------

data Context = Context ConEnvironment State
             deriving (Show)
type ConEnvironment = (Environment, [Ident], Integer)
type Environment = M.Map Ident Definition
type State = M.Map Ident [(Ident,Position)]

-------------------- Working on the DS ---------------------

createEmptyContext :: Integer -> Context
createEmptyContext n = Context (M.empty, [], n) M.empty

lookupViews :: Ident -> Environment -> [Ident]
lookupViews key env =
  case lookupKey key env of
    (Viewdef view _ _) ->
      [view]
    (Group _ views) ->
      views
    _ ->
      error $ "Tried to look up "++key++" found something not expected"

bindCommand :: Ident -> [(Ident,Position)] -> State -> State
bindCommand = M.insert

addToState :: (State -> State) -> Context -> State
addToState f (Context _ state) = f state

bindDefinition :: Ident -> Definition -> Environment -> Environment
bindDefinition = M.insert

addToEnvironment :: (Environment -> Environment) -> Context -> Context
addToEnvironment f (Context (env,active,fr) state) =
   Context (f env,active,fr) state

updateActiveViews :: [Ident] -> Context -> Context
updateActiveViews views (Context (env,_,fr) state) =
   Context (env,views,fr) state

placeShapeInActiveViews :: Definition -> Context -> Context
placeShapeInActiveViews (Rectangle id_ (Const x) (Const y) _ _ _) con =
  placeShapeHelper id_ (x,y) con
placeShapeInActiveViews (Circle id_ (Const x) (Const y) _ _) con =
  placeShapeHelper id_ (x,y) con
placeShapeInActiveViews _ _ = error "Trying to place something that is not a shape in the active views!"

placeShapeHelper :: Ident -> Position -> Context -> Context
placeShapeHelper id_ pos con =
  let (Context (env,active,fr) state) = con
      positions = map (\view_id -> (view_id,pos)) active
      newState = M.insert id_ positions state
  in
   Context (env,active,fr) newState
   
----------------- Working on Animation ---------------------

goToNextFrame :: Animation -> Animation
goToNextFrame (views, frames) = (views,frames++[[]])

addInstructions :: [GpxInstr] -> Animation -> Animation
addInstructions new_instr (views, frames) =
  let (rest,[curframe]) = getLast frames
  in
   (views, rest++[curframe++new_instr])

placeShapeInCurrentFrame :: Definition -> [ViewName] -> Animation -> Animation
placeShapeInCurrentFrame (Rectangle _ (Const x) (Const y) (Const w) (Const h) colour) active anim =
  placeShapeFrameHelper (DrawRect x y w h) colour active anim
placeShapeInCurrentFrame (Circle _ (Const x) (Const y) (Const r) colour) active anim =
  placeShapeFrameHelper (DrawCirc x y r) colour active anim
placeShapeInCurrentFrame _ _ _ = error "Trying to place something that is not a shape in the current frame!"

placeShapeFrameHelper :: (a -> ColourName -> GpxInstr)
                         -> Colour -> [a] -> Animation -> Animation
placeShapeFrameHelper shape colour active anim =
  let col = evalColour colour
      new_instr = map f active
      f view = shape view col
  in
   addInstructions new_instr anim
   
addViewToAnimation :: (ViewName,Integer,Integer) -> Animation -> Animation
addViewToAnimation v (views, frames) = (views++[v],frames)

------------------------------------------------------------
------------------------ Monads ----------------------------
------------------------------------------------------------

------------------------------------------------------------
--------------------- Monads Types -------------------------


--------------------- SalsaCommand -------------------------

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a,State)}

instance Monad SalsaCommand where
  return k = SalsaCommand $ \(Context _ state) -> (k,state)
  m >>= f = SalsaCommand $ \c ->
    let (Context e _) = c
        (a,state1) = runSC m c
        m1 = f a
    in
     runSC m1 (Context e state1)

------------------------ Salsa -----------------------------

data Salsa a = Salsa ((Context,Animation) -> (a,(Context,Animation)))

instance Monad Salsa where
  return k = Salsa $ \state -> (k,state)
  (Salsa a1) >>= f = Salsa $ \state0 -> let (r,state1) = a1 state0
                                            (Salsa a2) = f r
                                        in
                                         a2 state1

------------------------------------------------------------
--------------------- Accessors ----------------------------

--------------------- SalsaCommand -------------------------

askCmd :: SalsaCommand Context
askCmd = SalsaCommand $ \con@(Context _ s) -> (con,s)

updateState :: (Context -> State) -> SalsaCommand ()
updateState f = SalsaCommand $ \con -> ((), f con)

------------------------ Salsa -----------------------------

askCont :: Salsa Context
askCont = Salsa $ \s@(con,_) -> (con,s)

runSalsa :: Context -> Salsa a -> (a,(Context,Animation))
runSalsa con (Salsa m) = m (con,([],[[]]))

updateContext :: (Context -> Context) -> Salsa ()
updateContext f = Salsa $ \(con,anim) -> ((), (f con,anim))

updateAnimation :: (Animation -> Animation) -> Salsa ()
updateAnimation f = Salsa $ \(con,anim) -> ((), (con, f anim))

------------------------------------------------------------
---------------- Interpret Functions -----------------------
------------------------------------------------------------

--------------------- SalsaCommand -------------------------

command :: Command -> SalsaCommand ()
command (Move ids point) = do
  con <- askCmd
  let (Context (_,active,_) state) = con
  (h:_) <- mapM (\x ->
                  do
                    let list = lookupKey x state
                    newlist <- mapM (setNextPosition active point) list
                    updateState (addToState (bindCommand x newlist))
                ) ids
  return h -- Dummy return
command (At cmd id_) = do
  (Context (env,active,_) state) <- askCmd
  let vs = lookupViews id_ env
  (tmp_state,mapping) <- setTmpActiveViews state active vs
  updateState $ const tmp_state
  command cmd
  (Context (_,_,_) state1) <- askCmd
  next_state <- revertActiveViews state1 mapping
  updateState $ const next_state
command (Par cmd1 cmd2) = do
  command cmd1
  command cmd2

------------------------ Salsa -----------------------------

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com cmd) = do
  con <- askCont
  let (_,state) = runSC (command cmd) con
      (Context e s) = con
      newcon = Context e state
  instr <- compareStates s state
  updateContext $ const newcon
  updateAnimation $ addInstructions instr
  updateAnimation goToNextFrame

definition :: Definition -> Salsa ()
definition (Viewdef id_ x y) = do
  valx <- evalExpr x askCont
  valy <- evalExpr y askCont
  if valx <  0 || valy < 0 then
    error "A view cannot be defined with a negative width or height!"
   else do
    updateContext (addToEnvironment (bindDefinition id_ $ Viewdef id_ (Const valx) (Const valy)))
    updateContext (updateActiveViews [id_])
    updateAnimation (addViewToAnimation (id_,valx,valy))
definition (View id_) = do
  (Context (env,_,_) _) <- askCont
  let views = lookupViews id_ env
  updateContext (updateActiveViews views)
definition (Group id_ views) = do
  updateContext (addToEnvironment (bindDefinition id_ $ Group id_ views))
  updateContext (updateActiveViews views)
definition (Rectangle id_ x y w h colour) = do
  valx <- evalExpr x askCont
  valy <- evalExpr y askCont
  valw <- evalExpr w askCont
  valh <- evalExpr h askCont
  if valw <  0 || valh < 0 then
    error "A rectangle cannot be defined with a negative width or height!"
   else
    let newRect = Rectangle id_ (Const valx) (Const valy) (Const valw) (Const valh) colour
    in do
      updateContext (addToEnvironment (bindDefinition id_ newRect))
      updateContext (placeShapeInActiveViews newRect)
      (Context (_,active,_) _) <- askCont
      updateAnimation (placeShapeInCurrentFrame newRect active)
definition (Circle id_ x y r colour) = do
  valx <- evalExpr x askCont
  valy <- evalExpr y askCont
  valr <- evalExpr r askCont
  if valr <  0 then
    error "A circle cannot be defined with a negative radius!"
   else
     let newCirc = Circle id_ (Const valx) (Const valy) (Const valr) colour
     in do
       updateContext (addToEnvironment (bindDefinition id_ newCirc))
       updateContext (placeShapeInActiveViews newCirc)
       (Context (_,active,_) _) <- askCont
       updateAnimation (placeShapeInCurrentFrame newCirc active)

-------------------- Interpret Helpers ---------------------
------------------------------------------------------------

--------------------- SalsaCommand -------------------------

evalNextPoint :: Pos -> Position -> SalsaCommand Position
evalNextPoint (Abs exp1 exp2) _ = do
  x <- evalExpr exp1 askCmd
  y <- evalExpr exp2 askCmd
  return (x,y)
evalNextPoint (Rel exp1 exp2) (x1,y1) = do
  x <- evalExpr exp1 askCmd
  y <- evalExpr exp2 askCmd
  return (x1+x,y1+y)

setTmpActiveViews :: State -> [Ident] -> [Ident] -> SalsaCommand (State,[((Ident,Ident),Ident)])
setTmpActiveViews state active tmp_active =
  let list = M.toList state
      (a:_) = active
      act = removeDouble active tmp_active
      (new_list, mappings) = foldl (f tmp_active a) ([],[]) list
      (new_list2,mappings2) = foldl (f act $ '_':a) ([],[]) new_list
      f from to (acc_def,acc_m) (id1,positions) = let
        (next_def,next_m) = foldl (g from to id1) ([],[]) positions
        in
         (acc_def++[(id1,next_def)],acc_m++next_m)
      g from to id2 (acc_xs,acc_ms) (view,pos) = if view `elem` from
                                                 then (acc_xs++[(to,pos)],acc_ms++[((id2, to), view)])
                                                 else (acc_xs++[(view,pos)],acc_ms)
    in
   return (M.fromList new_list2, mappings++mappings2)



revertActiveViews :: State -> [((Ident,Ident),Ident)] -> SalsaCommand State
revertActiveViews state mappings = do
  let list = M.toList state
      new_state = map (\(id_,views) ->
                        (id_,map (\(viewName,pos) ->
                                   case lookup (id_,viewName) mappings of
                                     Just previous ->
                                       (previous,pos)
                                     Nothing ->
                                       (viewName,pos)
                                 ) views)) list
  return $ M.fromList new_state

setNextPosition :: Eq t => [t] -> Pos ->
                   (t, Position) -> SalsaCommand (t, Position)
setNextPosition active point (view,pos) =
  if view `elem` active
  then do
    next <- evalNextPoint point pos
    return (view,next)
  else
    return (view,pos)

getLowestPosition :: Position -> (ViewName,Position) -> Position
getLowestPosition (l_x,l_y) (_,(x,y)) = let
  n_x = if x < l_x then x else l_x
  n_y = if y < l_y then y else l_y
  in
   (n_x,n_y)

------------------------ Salsa -----------------------------

-- This function assumes that the set of keys in old is the same as in new
compareStates :: State -> State -> Salsa [GpxInstr]
compareStates old_s new_s = let
  s1 = M.toList old_s
  s2 = M.toList new_s
  in
   do
     l <- mapM (fg s1) s2
     return $ concat l
   where
     fg old (ident,new_positions) =
       case lookup ident old of
         Just old_positions ->
           do
             (Context (env,_,framerate) _) <- askCont
             l <- generateInstructions (lookupKey ident env) framerate
                  old_positions new_positions
             return $ concat l
         Nothing ->
           error $ ident++" did not exist in the new state"

-- It is assumed that every viewname in old_p.. must also appear in new_p..
generateInstructions :: Definition -> Integer -> [(ViewName, Position)] -> [(ViewName, Position)] -> Salsa [Frame]
generateInstructions s fr oldPos =
  mapM (genInstrHelper s fr oldPos)
  where
    genInstrHelper shape framerate old_positions (viewName, new_pos) =
      case lookup viewName old_positions of
        Just old_pos ->
          if old_pos == new_pos
          then return []
          else mapM (positionToInstr shape viewName) (interpolate framerate old_pos new_pos)
        Nothing ->
          error $ viewName++" did not exist in the list of old_positions"

positionToInstr :: Definition -> ViewName -> Position -> Salsa GpxInstr
positionToInstr (Rectangle _ _ _ expw exph expcol) viewName (x,y) = do
  w <- evalExpr expw askCont
  h <- evalExpr exph askCont
  return $ DrawRect x y w h viewName (evalColour expcol)
positionToInstr (Circle _ _ _ expr expcol) viewName (x,y) = do
  r <- evalExpr expr askCont
  return $ DrawCirc x y r viewName (evalColour expcol)
positionToInstr _ _ _ = error "Trying to create instructions from something that is not a shape"

evalExpr :: Monad m => Expr -> m Context -> m Integer
evalExpr (Const int) _ = return int
evalExpr (Plus exp1 exp2) askf = do
  x <- evalExpr exp1 askf
  y <- evalExpr exp2 askf
  return $ x+y
evalExpr (Minus exp1 exp2) askf = do
  x <- evalExpr exp1 askf
  y <- evalExpr exp2 askf
  return $ x-y
evalExpr (Xproj ident) askf = do
  (Context _ state) <- askf
  let max_i = toInteger(maxBound :: Int)
      (x,_) = foldl getLowestPosition (max_i,max_i) $ lookupKey ident state
  return x
evalExpr (Yproj ident) askf = do
  (Context _ state) <- askf
  let max_i = toInteger(maxBound :: Int)
      (_,y) = foldl getLowestPosition (max_i,max_i) $ lookupKey ident state
  return y
  
------------------------------------------------------------
---------------- Helper Functions --------------------------
------------------------------------------------------------

lookupKey :: Ord a => a -> M.Map a b -> b
lookupKey key env =
  Mb.fromMaybe (error "Tried to look up unknown key ")
  (M.lookup key env)


removeDouble :: Eq a => [a] -> [a] -> [a]
removeDouble [] _ = []
removeDouble _ [] = []
removeDouble (x:xs) ys =
  if x `elem` ys
  then removeDouble xs ys
  else x:removeDouble xs ys

evalColour :: Colour -> ColourName
evalColour Blue = "blue"
evalColour Plum = "plum"
evalColour Red = "red"
evalColour Green = "green"
evalColour Orange = "orange"

getLast :: [a] -> ([a],[a])
getLast [] = ([],[])
getLast (x:[]) = ([],[x])
getLast (x:xs) = let (rest,last_) = getLast xs
                 in
                  (x:rest,last_)
