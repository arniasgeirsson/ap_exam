module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import qualified Data.Map as M

--
-- The function interpolate
--

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


-- newtype SalsaT a = SalsaT { runSalsaT :: Salsa (SalsaCommand a)}

-- instance Monad SalsaT where
--   return x= SalsaT (return x)
  
-- The enivronment must be read-only
-- The state must be read/write
data Context = Context ConEnvironment State
             deriving (Show)
-- The environment bindings, list of active views, framerate 
type ConEnvironment = (Environment, [Ident], Integer)
type Environment = M.Map Ident Definition
-- The state; (shape_id, view_id) position::(int,int)
type State = M.Map Ident [(Ident,Position)]


createEmptyContext :: Integer -> Context
createEmptyContext n = Context (M.empty, [], n) M.empty

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a,State)}

instance Monad SalsaCommand where
  return k = SalsaCommand $ \(Context _ state) -> (k,state)
  m >>= f = SalsaCommand $ \c ->
    let (Context e s0) = c
        (a,state1) = runSC m c
        m1 = f a
    in
     runSC m1 (Context e state1)


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

-- functions for manipulating the context



--
-- Define the function command
--

-- COM I assume that the first defcom will always be a view definition
-- As it does not make sense to perfom any operations in a non-existing view

--lookupKey :: Ord a => a -> M.Map a b -> b
lookupKey key env = case M.lookup key env
                    of Just b -> b
                       Nothing -> error ("Tried to look up unknown key "++key)

askCmd :: SalsaCommand Context
askCmd = SalsaCommand $ \con@(Context _ s) -> (con,s)

command :: Command -> SalsaCommand ()
command (Move ids point) = do
  con <- askCmd
  let (Context (_,active,_) state) = con
  (h:_) <- mapM (\x ->
                  do
                    let list = lookupKey x state
                    newlist <- mapM (f active point) list
                    updateState (addToState (bindCommand x newlist))
                ) ids
  return h -- Dummy return
command (At cmd id) = do
  (Context (env,active,_) state) <- askCmd
  let vs = lookupViews id env
  (tmp_state,mapping) <- setTmpActiveViews state active vs
  updateState (\_ -> tmp_state)
  command cmd
  (Context (_,_,_) state1) <- askCmd
  next_state <- revertActiveViews state1 mapping
  updateState (\_ -> next_state)
command (Par cmd1 cmd2) = do
  command cmd1
  command cmd2

setTmpActiveViews :: State -> [Ident] -> [Ident] -> SalsaCommand (State,[((Ident,Ident),Ident)])
setTmpActiveViews state active tmp_active = do
  let list = M.toList state
      (a:as) = active
      (t:ts) = tmp_active
      act = removeDouble active tmp_active
      t_act = removeDouble tmp_active active
      (new_list, mappings) = foldl (f t_act a) ([],[]) list
      (new_list2,mappings2) = foldl (f act ("_"++a)) ([],[]) new_list
      f from to (acc_def,acc_m) (id,positions) = let
        (next_def,next_m) = foldl (g from to id) ([],[]) positions
        in
         (acc_def++[(id,next_def)],acc_m++next_m)
      g from to id (acc_xs,acc_ms) (view,pos) = if view `elem` from
                                                then (acc_xs++[(to,pos)],(acc_ms++[((id, to), view)]))
                                                else (acc_xs++[(view,pos)],acc_ms)
    in
   return (M.fromList new_list2, mappings++mappings2)


  
removeDouble :: [Ident] -> [Ident] -> [Ident]
removeDouble [] _ = []
removeDouble _ [] = []
removeDouble (x:xs) ys =
  if x `elem` ys
  then removeDouble xs ys
  else x:removeDouble xs ys

revertActiveViews :: State -> [((Ident,Ident),Ident)] -> SalsaCommand State
revertActiveViews state mappings = do
  let list = M.toList state
      new_state = map (\(id,views) -> (id,map (\(viewName,pos) -> case lookup (id,viewName) mappings of
                                                  Just previous ->
                                                    (previous,pos)
                                                  Nothing ->
                                                    (viewName,pos)
                                              ) views)) list
  return $ M.fromList new_state

-- COM Xproj and Yproj, my impl vs real impl
  
lookupViews key env =
  case lookupKey key env of
    (Viewdef view _ _) ->
      [view]
    (Group _ views) ->
      views
    _ ->
      error $ "Tried to look up "++key++" found something not expected"

-- COM what to if trying to move a shape in an active where it has not been before?
-- where should it start from?
-- -> In this case the shape is not moved
-- -> COM/ASSUM a shape that has not been defined within a view A can never appear in A
-- COM what to do with a shape in a frame if it not used in that frame?

-- COM I assume that when defining groups the same view cannot appear twice

--f :: (Eq t, Monad m) =>
--     [t] -> Pos -> (t, (Integer, Integer)) -> m (t, (Integer, Integer))
f :: Eq t =>
     [t]
     -> Pos
     -> (t, (Integer, Integer))
     -> SalsaCommand (t, (Integer, Integer))
f active point (view,pos) =
  case view `elem` active of
    True ->
      do
        next <- (evalNextPoint point pos)
        return $ (view,next)
    False ->
      return $ (view,pos)

evalNextPoint (Abs exp1 exp2) _ = do
  x <- evalExpr_ exp1
  y <- evalExpr_ exp2
  return $ (x,y)
evalNextPoint (Rel exp1 exp2) (x1,y1) = do
  x <- evalExpr_ exp1
  y <- evalExpr_ exp2
  return $ (x1+x,y1+y)
  
-- --f :: (ViewName,Position) ->
-- f (name,pos) = do  
--   case name `elem` active of
--     true -> (name,addPos pos point)
--             false -> (name,pos))

--  updateState (addToState (bindCommand (x,"stine") (0,0)))
--  updateState (addToState (bindCommand (x,"arni") (0,0)))

-- addPos :: Position -> Pos -> Position
-- addPos _ (Abs exp1 exp2) =
--   let x2 = evalExpr_ exp1
--       y2 = evalExpr_ exp2
--   in
--    (x2,y2)
-- addPos (x1,y1) (Rel exp1 exp2) =
--   let x2 = evalExpr_ exp1
--       y2 = evalExpr_ exp2
--   in
--    (x2,y2)


updateState :: (Context -> State) -> SalsaCommand ()
updateState f = SalsaCommand $ \con -> ((), f con)

bindCommand :: Ident -> [(Ident,Position)] -> State -> State
bindCommand = M.insert

addToState :: (State -> State) -> Context -> State
addToState f con =
  let (Context (env,active,fr) state) = con
      state' = f state
  in
   state'

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

definition :: Definition -> Salsa ()
definition (Viewdef id x y) = do
  valx <- evalExpr x
  valy <- evalExpr y
  updateContext (addToEnvironment (bindDefinition id $ Viewdef id (Const valx) (Const valy)))
  updateContext (updateActiveViews [id])
  updateAnimation (addViewToAnimation id valx valy)
definition (View id) = do
  (Context (env,_,_) _) <- askCont
  let views = lookupViews id env
  updateContext (updateActiveViews views)
definition (Group id views) = do
  updateContext (addToEnvironment (bindDefinition id $ Group id views))
  updateContext (updateActiveViews views)
definition (Rectangle id x y w h colour) = do
  valx <- evalExpr x
  valy <- evalExpr y
  valw <- evalExpr w
  valh <- evalExpr h
  if (valw <  0 || valh < 0) then
    error "A rectangle cannot be defined with a negative width or height!"
   else
    let newRect = Rectangle id (Const valx) (Const valy) (Const valw) (Const valh) colour
    in do
      updateContext (addToEnvironment (bindDefinition id newRect))
      updateContext (placeShapeInActiveViews newRect)
      ((Context (_,active,_) _),_) <- ask
      updateAnimation (placeShapeInCurrentFrame newRect active)
definition (Circle id x y r colour) = do
  valx <- evalExpr x
  valy <- evalExpr y
  valr <- evalExpr r
  if (valr <  0) then
    error "A circle cannot be defined with a negative radius!"
   else
     let newCirc = Circle id (Const valx) (Const valy) (Const valr) colour
     in do
       updateContext (addToEnvironment (bindDefinition id newCirc))
       updateContext (placeShapeInActiveViews newCirc)
       ((Context (_,active,_) _),_) <- ask
       updateAnimation (placeShapeInCurrentFrame newCirc active)



placeShapeInCurrentFrame :: Definition -> [ViewName] -> Animation -> Animation
placeShapeInCurrentFrame (Rectangle id (Const x) (Const y) (Const w) (Const h) colour) active anim =
  let (views, frames) = anim
      col = evalColour colour
      (rest,[curframe]) = getLast frames
      new_instr = map f active
      f view = (DrawRect x y w h view col)
  in
   (views,rest++[(curframe++new_instr)])
placeShapeInCurrentFrame (Circle id (Const x) (Const y) (Const r) colour) active anim =
  let (views, frames) = anim
      col = evalColour colour
      (rest,[curframe]) = getLast frames
      new_instr = map f active
      f view = (DrawCirc x y r view col)
  in
   (views,rest++[(curframe++new_instr)])
   
placeShapeInActiveViews :: Definition -> Context -> Context
placeShapeInActiveViews (Rectangle id (Const x) (Const y) w h colour) con =
  let (Context (env,active,fr) state) = con
      positions = map (\view_id -> (view_id,(x,y))) active
      newState = M.insert id positions state
  in
   (Context (env,active,fr) newState)
placeShapeInActiveViews (Circle id (Const x) (Const y) r colour) con =
  let (Context (env,active,fr) state) = con
      positions = map (\view_id -> (view_id,(x,y))) active
      newState = M.insert id positions state
  in
   (Context (env,active,fr) newState)

addViewToAnimation :: ViewName -> Integer -> Integer -> Animation -> Animation
addViewToAnimation id x y anim =
  let (views, frames) = anim
  in
   (views++[(id,x,y)],frames)

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

xValue (Rectangle _ expx _ _ _ _) = evalExpr expx
xValue (Circle _ expx _ _ _) = evalExpr expx
xValue _ = error "Trying to get the x coordinate of somethat that is not a shape"

yValue (Rectangle _ _ expy _ _ _) = evalExpr expy
yValue (Circle _ _ expy _ _) = evalExpr expy
yValue _ = error "Trying to get the y coordinate of somethat that is not a shape"

-- COM one of the failures of not having transform monad..
-- Fix by passing env around?
-- I can assume that only Salsa uses the evalExpr, when stuff is defined..
-- -> no expressions are also used when moving to a point..
-- -> although Salsa could eval that point?
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

xValue_ (Rectangle _ expx _ _ _ _) = evalExpr_ expx
xValue_ (Circle _ expx _ _ _) = evalExpr_ expx
xValue_ _ = error "Trying to get the x coordinate of somethat that is not a shape"

yValue_ (Rectangle _ _ expy _ _ _) = evalExpr_ expy
yValue_ (Circle _ _ expy _ _) = evalExpr_ expy
yValue_ _ = error "Trying to get the y coordinate of somethat that is not a shape"

evalColour Blue = "blue"
evalColour Plum = "plum"
evalColour Red = "red"
evalColour Green = "green"
evalColour Orange = "orange"
--evalColour _ = error "Trying to evaluate unrecognized colour"

getLast :: [a] -> ([a],[a])
getLast [] = ([],[])
getLast (x:[]) = ([],[x])
getLast (x:xs) = let (rest,last) = getLast xs
                 in
                  (x:rest,last)

bindDefinition :: Ident -> Definition -> Environment -> Environment
bindDefinition = M.insert

addToEnvironment :: (Environment -> Environment) -> Context -> Context
addToEnvironment f con =
  let (Context (env,active,fr) state) = con
      env' = f env
  in
   (Context (env',active,fr) state)

updateContext :: (Context -> Context) -> Salsa ()
updateContext f = Salsa $ \(con,anim) -> ((), (f con,anim))

updateAnimation :: (Animation -> Animation) -> Salsa ()
updateAnimation f = Salsa $ \(con,anim) -> ((), (con, f anim))

updateActiveViews :: [Ident] -> Context -> Context
updateActiveViews views con =
  let (Context (env,_,fr) state) = con
  in
   (Context (env,views,fr) state)

-- COM I only draw shapes that have moved
-- -> But then what if a command is a par and a shape is moved out then back?
-- -> I catch the par already in defCom and make sure that every move is handled
-- allowing me to only draw shapes that have moved, as I will do this with every com in par
-- ..... !!! I cannot (always) catch a Par command as it could be nested in an At or even in an Par!!!
-- TODO how to fix that?..
-- Let the SalsaCommand, report back that something havnt been parsed?

-- COM this function assumes that the set of keys in old is the same as in new
compareStates old new = let
  s1 = M.toList old
  s2 = M.toList new
  in
   do
     l <- mapM (fg s1) s2
     return $ concat l
   
fg old (ident,new_positions) = do
  case lookup ident old of
    Just old_positions ->
      do
        ((Context (env,_,framerate) _),_) <- ask
        l <- generateInstructions (lookupKey ident env) framerate
             old_positions new_positions
        let l2 = (concat l)
        return l2
    Nothing ->
      error $ ident++" did not exist in the new state"

fetchColour (Rectangle _ _ _ _ _ col) = evalColour col

-- COM it is assumed that every viewname in old_p.. must also appear in new_p..
generateInstructions shape framerate old_positions new_positions = do
  mapM (jm shape framerate old_positions) new_positions
  

jm shape framerate old_positions (viewName, new_pos) = do
  case lookup viewName old_positions of
    Just old_pos ->
      if old_pos == new_pos
      then return []
      else mapM (positionToInstr shape viewName) (interpolate framerate old_pos new_pos)
    Nothing ->
      error $ viewName++" did not exist in the list of old_positions"

positionToInstr (Rectangle _ _ _ expw exph expcol) viewName (x,y) = do
  w <- evalExpr expw
  h <- evalExpr exph
  return $ DrawRect x y w h viewName (evalColour expcol)
positionToInstr (Circle _ _ _ expr expcol) viewName (x,y) = do
  r <- evalExpr expr
  return $ DrawCirc x y r viewName (evalColour expcol)
  
goToNextFrame :: Animation -> Animation
-- COM should any thing be drawn to begin with? No, each frame starts clean/white/empty
goToNextFrame (views, frames) = (views,(reverse ([]:(reverse frames))))

addInstructions :: [GpxInstr] -> Animation -> Animation
addInstructions new_instr (views, frames) =
  let (rest,[curframe]) = getLast frames
  in
   (views, rest++[(curframe++new_instr)])

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com cmd) = do
  con1 <- ask
  let (con,anim) = con1
      (a,state) = runSC (command cmd) con
      (Context e s) = con
      newcon = (Context e state)
  instr <- compareStates s state
  updateContext (\_ -> newcon)
  updateAnimation $ addInstructions instr
  updateAnimation goToNextFrame

--
-- Define the function runProg
--


-- COM what to do with mutiply At/@ 's ?
-- -> I have decided that when encountering an At the previous view is stored
--    then it is updated to the given one and then the cmd is evaluated
--    meaning that if the next cmd is an At then that will store our change
--    and then update it again, meaning that no effect will happen in our view

-- read the state/context
ask :: Salsa (Context,Animation)
ask = Salsa $ \state -> (state,state)

askAnim :: Salsa Animation
askAnim = Salsa $ \s@(con,anim) -> (anim,s)

askCont :: Salsa Context
askCont = Salsa $ \s@(con,anim) -> (con,s)

-- updateSalsa :: ((Context,Animation) -> (a,(Context,Animation))) -> Salsa a
-- updateSalsa f = Salsa $ \state -> f state

runSalsa :: Context -> Salsa a -> (a,(Context,Animation))
runSalsa con (Salsa m) = m (con,([],[[]]))

--runProg :: Integer -> Program -> Animation
runProg n p =
  let (a,(con,anim)) = runSalsa (createEmptyContext n)
                   ( (join (Salsa $ \con -> (mapM defCom p, con))))
      join mm = do
        m <- mm
        m      
  in
   (anim)

testv = Def (Viewdef "Default" (Const 400) (Const 400))

testProg0 = [ Def (Viewdef "Default" (Const 400) (Const 400))
            , Def (Circle "b" (Const 10) (Const 10) (Const 5) Blue)
            , Def (Viewdef "A" (Const 1) (Const 2))
      --      , Def (View "Default")
            , Def (Group "C" ["A", "B","C"])
            , Def (Rectangle "a" (Const 1) (Const 1) (Const 20) (Const 40) Green)
        --    , Com (Move ["b"] (Abs (Const 20) (Const 40)))
            , Com (Move ["a","b"] (Abs (Const 20) (Const 50)))
            , Com (At (Move ["b"] (Abs (Const 100) (Const (200)))) "Default")
            , Com (Move ["a"] (Abs (Const 600) (Const 1000)))
            ]

testAnim0 = ([("Default", 400, 400)], [[(DrawRect 10 400 20 20 "Default" "green")]])
