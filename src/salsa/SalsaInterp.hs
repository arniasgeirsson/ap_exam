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

------------------------------------------------------------
------------------ The interface ---------------------------
------------------------------------------------------------

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

runProg :: Integer -> Program -> Animation
runProg n p =
  let (_,(_,anim)) = runSalsa (createEmptyContext n)
                   (join (Salsa $ \con -> (mapM defCom p, con)))
      join mm = do
        m <- mm
        m      
  in
   anim

------------------------------------------------------------
--------------------- Context ------------------------------
------------------------------------------------------------

-- Data Structure -- 

-- The enivronment must be read-only
-- The state must be read/write
data Context = Context ConEnvironment State
             deriving (Show)
-- The environment bindings, list of active views, framerate 
type ConEnvironment = (Environment, [Ident], Integer)
type Environment = M.Map Ident Definition
-- The state; (shape_id, view_id) position::(int,int)
type State = M.Map Ident [(Ident,Position)]

-- Working on it

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
addToState f con =
  let (Context _ state) = con
      state' = f state
  in
   state'

bindDefinition :: Ident -> Definition -> Environment -> Environment
bindDefinition = M.insert

addToEnvironment :: (Environment -> Environment) -> Context -> Context
addToEnvironment f con =
  let (Context (env,active,fr) state) = con
      env' = f env
  in
   Context (env',active,fr) state



updateActiveViews :: [Ident] -> Context -> Context
updateActiveViews views con =
  let (Context (env,_,fr) state) = con
  in
   Context (env,views,fr) state

placeShapeInActiveViews :: Definition -> Context -> Context
placeShapeInActiveViews (Rectangle id_ (Const x) (Const y) _ _ _) con =
  let (Context (env,active,fr) state) = con
      positions = map (\view_id -> (view_id,(x,y))) active
      newState = M.insert id_ positions state
  in
   Context (env,active,fr) newState
placeShapeInActiveViews (Circle id_ (Const x) (Const y) _ _) con =
  let (Context (env,active,fr) state) = con
      positions = map (\view_id -> (view_id,(x,y))) active
      newState = M.insert id_ positions state
  in
   Context (env,active,fr) newState
placeShapeInActiveViews _ _ = error "Trying to place something that is not a shape in the active views!"

-- Working with Animation -- 


goToNextFrame :: Animation -> Animation
-- COM should any thing be drawn to begin with? No, each frame starts clean/white/empty
goToNextFrame (views, frames) = (views,reverse ([]:reverse frames))

addInstructions :: [GpxInstr] -> Animation -> Animation
addInstructions new_instr (views, frames) =
  let (rest,[curframe]) = getLast frames
  in
   (views, rest++[curframe++new_instr])

placeShapeInCurrentFrame :: Definition -> [ViewName] -> Animation -> Animation
placeShapeInCurrentFrame (Rectangle _ (Const x) (Const y) (Const w) (Const h) colour) active anim =
  let (views, frames) = anim
      col = evalColour colour
      (rest,[curframe]) = getLast frames
      new_instr = map f active
      f view = DrawRect x y w h view col
  in
   (views,rest++[curframe++new_instr])
placeShapeInCurrentFrame (Circle _ (Const x) (Const y) (Const r) colour) active anim =
  let (views, frames) = anim
      col = evalColour colour
      (rest,[curframe]) = getLast frames
      new_instr = map f active
      f view = DrawCirc x y r view col
  in
   (views,rest++[curframe++new_instr])
placeShapeInCurrentFrame _ _ _ = error "Trying to place something that is not a shape in the current frame!"



addViewToAnimation :: ViewName -> Integer -> Integer -> Animation -> Animation
addViewToAnimation id_ x y anim =
  let (views, frames) = anim
  in
   (views++[(id_,x,y)],frames)

------------------------------------------------------------
--------------------- Monads -------------------------------
------------------------------------------------------------

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a,State)}

instance Monad SalsaCommand where
  return k = SalsaCommand $ \(Context _ state) -> (k,state)
  m >>= f = SalsaCommand $ \c ->
    let (Context e _) = c
        (a,state1) = runSC m c
        m1 = f a
    in
     runSC m1 (Context e state1)

data Salsa a = Salsa ((Context,Animation) -> (a,(Context,Animation)))

instance Monad Salsa where
  return k = Salsa $ \state -> (k,state)
  (Salsa a1) >>= f = Salsa $ \state0 -> let (r,state1) = a1 state0
                                            (Salsa a2) = f r
                                        in
                                         a2 state1
-- Working with the monads --

-- Salsa -- 
-- read the state/context
ask :: Salsa (Context,Animation)
ask = Salsa $ \state -> (state,state)

-- TODO not used
-- askAnim :: Salsa Animation
-- askAnim = Salsa $ \s@(_,anim) -> (anim,s)

askCont :: Salsa Context
askCont = Salsa $ \s@(con,_) -> (con,s)

runSalsa :: Context -> Salsa a -> (a,(Context,Animation))
runSalsa con (Salsa m) = m (con,([],[[]]))

updateContext :: (Context -> Context) -> Salsa ()
updateContext f = Salsa $ \(con,anim) -> ((), (f con,anim))

updateAnimation :: (Animation -> Animation) -> Salsa ()
updateAnimation f = Salsa $ \(con,anim) -> ((), (con, f anim))

-- SalsaCommand --

askCmd :: SalsaCommand Context
askCmd = SalsaCommand $ \con@(Context _ s) -> (con,s)

updateState :: (Context -> State) -> SalsaCommand ()
updateState f = SalsaCommand $ \con -> ((), f con)


-- COM I assume that the first defcom will always be a view definition
-- As it does not make sense to perfom any operations in a non-existing view

------------------------------------------------------------
---------------- Interpret Functions -----------------------
------------------------------------------------------------

-- SalsaCommand --

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

evalNextPoint :: Pos -> Position -> SalsaCommand Position
evalNextPoint (Abs exp1 exp2) _ = do
  x <- evalExpr_ exp1
  y <- evalExpr_ exp2
  return (x,y)
evalNextPoint (Rel exp1 exp2) (x1,y1) = do
  x <- evalExpr_ exp1
  y <- evalExpr_ exp2
  return (x1+x,y1+y)


-- Salsa --

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com cmd) = do
  con1 <- ask
  let (con,_) = con1
      (_,state) = runSC (command cmd) con
      (Context e s) = con
      newcon = Context e state
  instr <- compareStates s state
  updateContext $ const newcon
  updateAnimation $ addInstructions instr
  updateAnimation goToNextFrame

definition :: Definition -> Salsa ()
definition (Viewdef id_ x y) = do
  valx <- evalExpr x
  valy <- evalExpr y
  updateContext (addToEnvironment (bindDefinition id_ $ Viewdef id_ (Const valx) (Const valy)))
  updateContext (updateActiveViews [id_])
  updateAnimation (addViewToAnimation id_ valx valy)
definition (View id_) = do
  (Context (env,_,_) _) <- askCont
  let views = lookupViews id_ env
  updateContext (updateActiveViews views)
definition (Group id_ views) = do
  updateContext (addToEnvironment (bindDefinition id_ $ Group id_ views))
  updateContext (updateActiveViews views)
definition (Rectangle id_ x y w h colour) = do
  valx <- evalExpr x
  valy <- evalExpr y
  valw <- evalExpr w
  valh <- evalExpr h
  if valw <  0 || valh < 0 then
    error "A rectangle cannot be defined with a negative width or height!"
   else
    let newRect = Rectangle id_ (Const valx) (Const valy) (Const valw) (Const valh) colour
    in do
      updateContext (addToEnvironment (bindDefinition id_ newRect))
      updateContext (placeShapeInActiveViews newRect)
      (Context (_,active,_) _,_) <- ask
      updateAnimation (placeShapeInCurrentFrame newRect active)
definition (Circle id_ x y r colour) = do
  valx <- evalExpr x
  valy <- evalExpr y
  valr <- evalExpr r
  if valr <  0 then
    error "A circle cannot be defined with a negative radius!"
   else
     let newCirc = Circle id_ (Const valx) (Const valy) (Const valr) colour
     in do
       updateContext (addToEnvironment (bindDefinition id_ newCirc))
       updateContext (placeShapeInActiveViews newCirc)
       (Context (_,active,_) _,_) <- ask
       updateAnimation (placeShapeInCurrentFrame newCirc active)

-- COM it is assumed that every viewname in old_p.. must also appear in new_p..
generateInstructions :: Definition -> Integer -> [(ViewName, Position)] -> [(ViewName, Position)] -> Salsa [Frame]
generateInstructions shape framerate old_positions =
  mapM (jm shape framerate old_positions)
  
-- TODO rename!!
jm :: Definition -> Integer -> [(ViewName,Position)] -> (ViewName,Position) -> Salsa [GpxInstr]
jm shape framerate old_positions (viewName, new_pos) =
  case lookup viewName old_positions of
    Just old_pos ->
      if old_pos == new_pos
      then return []
      else mapM (positionToInstr shape viewName) (interpolate framerate old_pos new_pos)
    Nothing ->
      error $ viewName++" did not exist in the list of old_positions"

positionToInstr :: Definition -> ViewName -> Position -> Salsa GpxInstr
positionToInstr (Rectangle _ _ _ expw exph expcol) viewName (x,y) = do
  w <- evalExpr expw
  h <- evalExpr exph
  return $ DrawRect x y w h viewName (evalColour expcol)
positionToInstr (Circle _ _ _ expr expcol) viewName (x,y) = do
  r <- evalExpr expr
  return $ DrawCirc x y r viewName (evalColour expcol)
positionToInstr _ _ _ = error "Trying to create instructions from something that is not a shape"

-- Interpret Helpers --

-- SalsaCommand --

setTmpActiveViews :: State -> [Ident] -> [Ident] -> SalsaCommand (State,[((Ident,Ident),Ident)])
setTmpActiveViews state active tmp_active =
  let list = M.toList state
      (a:_) = active
      act = removeDouble active tmp_active
      t_act = removeDouble tmp_active active
      (new_list, mappings) = foldl (f t_act a) ([],[]) list
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
      new_state = map (\(id_,views) -> (id_,map (\(viewName,pos) -> case lookup (id_,viewName) mappings of
                                                  Just previous ->
                                                    (previous,pos)
                                                  Nothing ->
                                                    (viewName,pos)
                                              ) views)) list
  return $ M.fromList new_state

setNextPosition :: Eq t => [t] -> Pos -> (t, (Integer, Integer)) -> SalsaCommand (t, (Integer, Integer))
setNextPosition active point (view,pos) =
  if view `elem` active
  then do
    next <- evalNextPoint point pos
    return (view,next)
  else
    return (view,pos)

-- COM one of the failures of not having transform monad..
-- Fix by passing env around?
-- I can assume that only Salsa uses the evalExpr, when stuff is defined..
-- -> no expressions are also used when moving to a point..
-- -> although Salsa could eval that point?
evalExpr_ :: Expr -> SalsaCommand Integer
evalExpr_ (Const int) = return int
evalExpr_ (Plus exp1 exp2) = do
  x <- evalExpr_ exp1
  y <- evalExpr_ exp2
  return $ x+y
evalExpr_ (Minus exp1 exp2) = do
  x <- evalExpr_ exp1
  y <- evalExpr_ exp2
  return $ x-y
evalExpr_ (Xproj ident) = do
  (Context (env,_,_) _) <- askCmd
  xValue_ $ lookupKey ident env
evalExpr_ (Yproj ident) = do
  (Context (env,_,_) _) <- askCmd
  yValue_ $ lookupKey ident env  

xValue_ :: Definition -> SalsaCommand Integer
xValue_ (Rectangle _ expx _ _ _ _) = evalExpr_ expx
xValue_ (Circle _ expx _ _ _) = evalExpr_ expx
xValue_ _ = error "Trying to get the x coordinate of somethat that is not a shape"

yValue_ :: Definition -> SalsaCommand Integer
yValue_ (Rectangle _ _ expy _ _ _) = evalExpr_ expy
yValue_ (Circle _ _ expy _ _) = evalExpr_ expy
yValue_ _ = error "Trying to get the y coordinate of somethat that is not a shape"


-- COM Xproj and Yproj, my impl vs real impl

-- Salsa --

-- COM what to if trying to move a shape in an active where it has not been before?
-- where should it start from?
-- -> In this case the shape is not moved
-- -> COM/ASSUM a shape that has not been defined within a view A can never appear in A
-- COM what to do with a shape in a frame if it not used in that frame?

-- COM I assume that when defining groups the same view cannot appear twice

--
-- Define the functions liftC, definition, and defCom
--

--liftC :: SalsaCommand a -> Salsa a
-- liftC m = 
--   do
--     con <- ask
--     let (_,state) = runSC m con
--         (Context e s) = con
--         newcon = (Context e state)
--     updateContext (\_ -> newcon)
--     updateSalsa something


evalExpr :: Expr -> Salsa Integer
evalExpr (Const int) = return int
evalExpr (Plus exp1 exp2) = do
  x <- evalExpr exp1
  y <- evalExpr exp2
  return $ x+y
evalExpr (Minus exp1 exp2) = do
  x <- evalExpr exp1
  y <- evalExpr exp2
  return $ x-y
evalExpr (Xproj ident) = do
  (Context (env,_,_) _) <- askCont
  xValue $ lookupKey ident env
evalExpr (Yproj ident) = do
  (Context (env,_,_) _) <- askCont
  yValue $ lookupKey ident env  

xValue :: Definition -> Salsa Integer
xValue (Rectangle _ expx _ _ _ _) = evalExpr expx
xValue (Circle _ expx _ _ _) = evalExpr expx
xValue _ = error "Trying to get the x coordinate of somethat that is not a shape"

yValue :: Definition -> Salsa Integer
yValue (Rectangle _ _ expy _ _ _) = evalExpr expy
yValue (Circle _ _ expy _ _) = evalExpr expy
yValue _ = error "Trying to get the y coordinate of somethat that is not a shape"

-- COM this function assumes that the set of keys in old is the same as in new
compareStates :: State -> State -> Salsa [GpxInstr]
compareStates old new = let
  s1 = M.toList old
  s2 = M.toList new
  in
   do
     l <- mapM (fg s1) s2
     return $ concat l

-- TODO rename!
fg :: [(Ident, [(ViewName, Position)])] -> (Ident, [(ViewName, Position)]) -> Salsa [GpxInstr]
fg old (ident,new_positions) =
  case lookup ident old of
    Just old_positions ->
      do
        (Context (env,_,framerate) _,_) <- ask
        l <- generateInstructions (lookupKey ident env) framerate
             old_positions new_positions
        let l2 = concat l
        return l2
    Nothing ->
      error $ ident++" did not exist in the new state"






-- COM I only draw shapes that have moved
-- -> But then what if a command is a par and a shape is moved out then back?
-- -> I catch the par already in defCom and make sure that every move is handled
-- allowing me to only draw shapes that have moved, as I will do this with every com in par
-- ..... !!! I cannot (always) catch a Par command as it could be nested in an At or even in an Par!!!
-- TODO how to fix that?..
-- Let the SalsaCommand, report back that something havnt been parsed?






  
------------------------------------------------------------
---------------- Helper Functions --------------------------
------------------------------------------------------------

lookupKey :: Ord a => a -> M.Map a b -> b
lookupKey key env =
  Mb.fromMaybe (error "Tried to look up unknown key ")
  (M.lookup key env)


removeDouble :: [Ident] -> [Ident] -> [Ident]
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

-- TODO not used
-- fetchColour :: Definition -> ColourName
-- fetchColour (Rectangle _ _ _ _ _ col) = evalColour col
-- fetchColour (Circle _ _ _ _ col) = evalColour col
-- fetchColour _ = error "Trying to fetch a colour from something that is not a shape"



getLast :: [a] -> ([a],[a])
getLast [] = ([],[])
getLast (x:[]) = ([],[x])
getLast (x:xs) = let (rest,last_) = getLast xs
                 in
                  (x:rest,last_)

-- COM what to do with mutiply At/@ 's ?
-- -> I have decided that when encountering an At the previous view is stored
--    then it is updated to the given one and then the cmd is evaluated
--    meaning that if the next cmd is an At then that will store our change
--    and then update it again, meaning that no effect will happen in our view



-- testv :: DefCom
-- testv = Def (Viewdef "Default" (Const 400) (Const 400))

-- testProg0 :: [DefCom]
-- testProg0 = [ Def (Viewdef "Default" (Const 400) (Const 400))
--             , Def (Circle "b" (Const 10) (Const 10) (Const 5) Blue)
--             , Def (Viewdef "A" (Const 1) (Const 2))
--       --      , Def (View "Default")
--             , Def (Group "C" ["A", "B","C"])
--             , Def (Rectangle "a" (Const 1) (Const 1) (Const 20) (Const 40) Green)
--         --    , Com (Move ["b"] (Abs (Const 20) (Const 40)))
--             , Com (Move ["a","b"] (Abs (Const 20) (Const 50)))
--             , Com (At (Move ["b"] (Abs (Const 100) (Const 200))) "Default")
--             , Com (Move ["a"] (Abs (Const 600) (Const 1000)))
--             ]

-- testAnim0 :: ([(String, Integer, Integer)], [[GpxInstr]])
-- testAnim0 = ([("Default", 400, 400)], [[DrawRect 10 400 20 20 "Default" "green"]])
