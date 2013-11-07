----------------------------------------------------------------------
--- Student name: Arni Asgeirsson
--- Student KU-id: lwf986
----------------------------------------------------------------------
module Test_Interp
       (runAllTestsI,runAllTestsIWith)
       where

import SalsaAst
import Gpx
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Control.Monad
import SalsaInterp
import qualified Data.Map as M
import Data.Char
import Data.List
import qualified Data.Maybe as Mb

------------------------------------------------------------
------------------ Interface to run tests ------------------
------------------------------------------------------------

runAllTestsI :: IO ()
runAllTestsI = runAllTestsIWith 100

runAllTestsIWith :: Int -> IO ()
runAllTestsIWith n = do
  putStrLn "------------------------------------------------------------------"
  putStrLn "------------ Running tests for the SalsaInterp module ------------"
  putStrLn "------------------------------------------------------------------\n"
  putStrLn "-------------------- Running QuickCheck tests --------------------"
  putStrLn $ "1. Testing if the interpreter inteprets the expected outputs from random"
    ++" valid Salsa Program"
  putStrLn "Might take a few seconds ...\n"
  runQCTest n

------------------------------------------------------------
--------------------- QuickCheck Tests ---------------------
---------------- Test valid input Programs -----------------
------------------------------------------------------------

----------------------- Definitions ------------------------

identarr :: String
identarr = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

-- COM group and at does not work together

definitions :: [String]
definitions = ["viewdef","rectangle", "circle", "view"] -- ,"group"]

commands :: [String]
commands = ["move",  "at",--"par",
            "move","move","move"]

colours :: [(String,Colour)]
colours = [("blue",Blue), ("plum",Plum), ("red",Red),
           ("green",Green), ("orange",Orange)]

numbers :: String
numbers = ['0'..'9']

exprList :: [String]
exprList = ["const","plus", "minus", "xproj", "yproj",
            "const","const","const","const","const"]

posList :: [String]
posList = ["abs","rel"]

------------------------ Test type -------------------------

newtype TestAnimation = TestAnimation ((Program,Integer), Animation)
                    deriving (Show, Eq)

instance QC.Arbitrary TestAnimation where
  arbitrary = do
    defcoms <- QC.listOf1 $ QC.elements $ definitions++commands
    (input,output_) <- genManyDefcom ("viewdef":defcoms)
    return $ TestAnimation (input, output_)

------------------------- Property -------------------------

prop_runProg :: TestAnimation -> Bool
prop_runProg (TestAnimation ((i,n),o)) = compareAnimations (runProg n i) o

---------------------- QC test runner ----------------------

-- TODO allow it to be user defined how many tests it must run
runQCTest :: Int -> IO ()
runQCTest n = QC.quickCheckWith QC.stdArgs{maxSuccess = n } prop_runProg

------------------------ Generators ------------------------


-- COM the framerate is intentionaly keept low to avoid very very big data sets
-- COM I assume that you cannot have group names in inside a group definition
-- COM I assume that no two names can be used to define two things


genManyDefcom :: [String] -> QC.Gen (([DefCom],Integer),Animation)
genManyDefcom [] = error "Cannot parse an empty list of definitions or commands"
genManyDefcom words_ = do
  n <- QC.elements ['1'..'9']
  let framerate = read [n]::Integer
      init_ = (createEmptyContext framerate,[],([],[[]]))
  (_,all_defcoms,all_anim) <- foldM f init_ words_
  return ((all_defcoms,framerate),all_anim)
  where
    f (acontext,acci,anim) word = do
      (context,defcoms,new_anim) <- genDefcom word acontext anim
      if word `elem` commands && defcoms /= []
        then
          let (view,frames) = new_anim
              next = reverse ([]:reverse frames)
          in
           return (context,acci++defcoms,(view,next))
        else
          return (context,acci++defcoms,new_anim)
genDefcom :: String -> Context -> Animation -> QC.Gen (Context,[DefCom],Animation)
genDefcom "viewdef" c@(Context (env,_,n) state) a@(views,frames) = do
  vident <- genVident
  if isInEnvironment vident env
    then
      return (c,[],a)
    else do
      expw <- genExpr env
      exph <- genExpr env
      w <- evalExprQC expw env
      h <- evalExprQC exph env
      let def = Viewdef vident expw exph
          env' = M.insert vident def env
      return (Context (env',[vident],n) state,[Def def], (views++[(vident,w,h)],frames))
genDefcom "rectangle" c@(Context (env,active,n) state) a@(views,frames) = do
  sident <- genSident
  if isInEnvironment sident env
    then
      return (c,[],a)
    else do
      expx <- genExpr env
      expy <- genExpr env
      expw_ <- genExpr env
      expw <- forcePositive expw_ env
      exph_ <- genExpr env
      exph <- forcePositive exph_ env      
      x <- evalExprQC expx env
      y <- evalExprQC expy env
      w <- evalExprQC expw env
      h <- evalExprQC exph env
      (col,col_type) <- genColour
      let (rest,[last_]) = getLast frames
          instrs = map (\viewName -> DrawRect x y w h viewName col) active
          def = Rectangle sident expx expy expw exph col_type
          env' = M.insert sident def env
          list = map (\viewName -> (viewName,(x,y))) active
          state' = M.insert sident list state
      return (Context (env',active,n) state',[Def def], (views,rest++[last_++instrs]))
genDefcom "circle" c@(Context (env,active,n) state) a@(views,frames) = do
  sident <- genSident
  if isInEnvironment sident env
    then
      return (c,[],a)
    else do
      expx <- genExpr env
      expy <- genExpr env
      expr_ <- genExpr env
      expr <- forcePositive expr_ env
      x <- evalExprQC expx env
      y <- evalExprQC expy env
      r <- evalExprQC expr env
      (col,col_type) <- genColour
      let (rest,[last_]) = getLast frames
          instrs = map (\viewName -> DrawCirc x y r viewName col) active
          def = Circle sident expx expy expr col_type
          env' = M.insert sident def env
          list = map (\viewName -> (viewName,(x,y))) active
          state' = M.insert sident list state
      return (Context (env',active,n) state',[Def def], (views,rest++[last_++instrs]))
genDefcom "view" c@(Context (env,_,n) state) a@anim =
  let list = M.toList env
      flist = filter (\(_,def) -> case def of
                         (Group _ _) -> True
                         (View _) -> True
                         _ -> False
                         ) list
  in
   if null flist
   then return (c,[], a)
   else do
     (id_,some_def) <- QC.elements flist
     let new_active = case some_def of
           (Group _ g) -> g
           (View v) -> [v]
           _ -> error "Shouldn't be possible due to filtering above"
     return (Context (env,new_active,n) state, [Def $ View id_],anim)
genDefcom "group" c@(Context (_,[],_) _) a =
  return (c,[],a)
genDefcom "group" c@(Context (env,active,n) state) a = do
  vident <- genVident
  if isInEnvironment vident env
    then
      return (c,[],a)
    else do
      new_actives_ <- QC.listOf1 $ QC.elements active
      let new_actives = removeDuplex new_actives_
          def = Group vident new_actives
          env' = M.insert vident def env
      return (Context (env',new_actives,n) state, [Def def], a)
genDefcom "move" c@(Context (env,active,n) state) a@(views,frames) =
  let list = M.toList env
      flist = filter (\(x:_,_) -> isLower x) list
  in
   if null flist
   then return (c,[], a)
   else do
     ids <- QC.listOf1 $ QC.elements flist
     let ids2 = removeDuplex ids
     expPos <- genPos env
     (all_instr,new_state) <- foldM (f_ active expPos n env) ([],state) ids2
     let (rest,[last_]) = getLast frames
         next1 = rest++[last_++all_instr]
         ids_ = map fst ids2
         def = Move ids_ expPos
     return (Context (env,active,n) new_state,[Com def],(views,next1))
genDefcom "at" c@(Context (env,active,n) state) a =
  let list = M.toList env
      flist = filter (\(_,def) -> case def of
                         (Group _ _) -> True
                         (View _) -> True
                         _ -> False
                         ) list
  in
   if null flist
   then return (c,[], a)
   else do
     (id_,some_def) <- QC.elements flist
     let tmp_active = case some_def of
           (Group _ g) -> g
           (View v) -> [v]
           _ -> error "Shouldn't be possible due to the filtering above"
     middle_cmd <- QC.elements commands
     middle <- genDefcom middle_cmd (Context (env,tmp_active,n) state) a
     let (Context (env',_,_) state', a_com, anim') = middle
     case a_com of
       [] ->
         return (c,[],a)
       [Com some] ->
         return (Context (env',active,n) state', [Com (At some id_)], anim')
       _ -> error "Shouldn't be possible"
-- genDefcom "par" = do
--   word1 <- QC.elements commands
--   word2 <- QC.elements commands
--   (cmd1,Com cmdexp1:[]) <- genDefcom word1
--   (cmd2,Com cmdexp2:[]) <- genDefcom word2
--   input <- insertWhiteSpaces ["{",cmd1,"||",cmd2,"}"]
--   return (input, [Com $ Par cmdexp1 cmdexp2])
genDefcom s _ _ = error $ "Cannot parse "++s++" into a DefCom"

forcePositive :: Expr -> Environment -> Gen Expr
forcePositive exp_ env = do
  val <- evalExprQC exp_ env
  return (if val >= 0
          then
            exp_
          else
            Plus exp_ (Const (val*(-2))))

-- COM assumes that id is in state
-- TODO RENAME
f_ :: Ord t => [ViewName]  -> Pos -> Integer -> Environment -> ([GpxInstr], M.Map t [(ViewName, (Integer, Integer))]) -> (t, Definition) -> Gen ([GpxInstr], M.Map t [(ViewName, (Integer, Integer))])
f_ active pos n env acc (id_,def) =
  foldM (\(acc_instrs, acc_state) a ->
           let positions = lookupKey id_ acc_state
           in
            case lookup a positions of
              Just old_pos -> do
                next_pos <- evalPos_ old_pos pos env
                let next_state = M.insert id_ (map (\(vn,p) -> if vn == a
                                                               then (vn,next_pos)
                                                               else (vn,p)
                                                   ) positions) acc_state
                instr <- genInstrs old_pos next_pos def n a env
                return (acc_instrs++instr,next_state)
              Nothing ->
                return (acc_instrs,acc_state)) acc active

genInstrs :: (Integer, Integer)
                   -> (Integer, Integer)
                   -> Definition
                   -> Integer
                   -> ViewName
                   -> Environment
                   -> Gen [GpxInstr]
genInstrs opos npos (Rectangle _ _ _ ew eh ecol) n view env = do
  w <- evalExprQC ew env
  h <- evalExprQC eh env
  col <- evalColourQc ecol
  let positions = interpolate n opos npos
  return $ map (\(x,y) -> DrawRect x y w h view col) positions
genInstrs opos npos (Circle _ _ _ er ecol) n view env = do
  r <- evalExprQC er env
  col <- evalColourQc ecol
  let positions = interpolate n opos npos
  return $ map (\(x,y) -> DrawCirc x y r view col) positions
genInstrs _ _ _ _ _ _ = error "Cannot generate instructions if not given a shape"

evalPos_ :: (Integer, Integer)
                  -> Pos -> Environment -> Gen (Integer, Integer)
evalPos_ _ (Abs expx expy) env = do
  x2 <- evalExprQC expx env
  y2 <- evalExprQC expy env
  return (x2,y2)
evalPos_ (x,y) (Rel expx expy) env = do
  x2 <- evalExprQC expx env
  y2 <- evalExprQC expy env
  return (x+x2,y+y2)

genPos :: Environment -> QC.Gen Pos
genPos env = do
  pos <- QC.elements posList
  _genPos pos env

_genPos :: String -> Environment -> QC.Gen Pos
_genPos "abs" env = do
  expx <- genExpr env
  expy <- genExpr env
  return (Abs expx expy)
_genPos "rel" env = do
  expx <- genExpr env
  expy <- genExpr env
  return (Rel expx expy)
_genPos s _ = error $ "Cannot parse "++s++" into an Pos"

evalColourQc :: Colour -> QC.Gen String
evalColourQc Blue = return "blue"
evalColourQc Plum = return "plum"
evalColourQc Red = return "red"
evalColourQc Green = return "green"
evalColourQc Orange = return "orange"

evalExprQC :: Expr -> Environment -> QC.Gen Integer
evalExprQC (Const n) _ = return n
evalExprQC (Plus e1 e2) env = do
  n1 <- evalExprQC e1 env
  n2 <- evalExprQC e2 env
  return $ n1 + n2
evalExprQC (Minus e1 e2) env = do
  n1 <- evalExprQC e1 env
  n2 <- evalExprQC e2 env
  return $ n1 - n2
evalExprQC (Xproj id_) env = do
  let def = lookupKey id_ env
      (expx,_) = getPosition def
  evalExprQC expx env
evalExprQC (Yproj id_) env = do
  let def = lookupKey id_ env
      (_,expy) = getPosition def
  evalExprQC expy env

genExpr :: Environment -> QC.Gen Expr
genExpr env = do
  expr <- QC.elements exprList
  _genExpr expr env

-- COM/NOTE if i have to create a xproj or yproj, but no shape definition
  -- has been made yet, a Const will be returned instead.
_genExpr :: String -> Environment -> QC.Gen Expr
_genExpr "plus" env = do
  (exp1) <- genExpr env
  (exp2) <- genExpr env
  return (Plus exp1 exp2)
_genExpr "minus" env = do
  (exp1) <- genExpr env
  (exp2) <- genExpr env
  return (Minus exp1 exp2)
_genExpr "const" _ = do
  n <- genNumber
  return (Const (read n::Integer))
_genExpr "xproj" env =
  let list = M.toList env
      flist = filter (\(x:_,_) -> isLower x) list
    in
   if null flist
   then
     _genExpr "const" env
   else do
     (sid,_) <- QC.elements flist
     return (Xproj sid)
_genExpr "yproj" env =
  let list = M.toList env
      flist = filter (\(x:_,_) -> isLower x) list
    in
   if null flist
   then
     _genExpr "const" env
   else do
     (sid,_) <- QC.elements flist
     return (Yproj sid)
_genExpr s _ = error $ "Cannot parse "++s++" into an Expr"

genColour :: QC.Gen (String,Colour)
genColour = QC.elements colours

genVident :: QC.Gen String
genVident = do
  h <- QC.elements ['A'..'Z']
  rest <- QC.listOf $ QC.elements identarr
  return $ h:rest

genSident :: QC.Gen String
genSident = do
  h <- QC.elements ['a'..'z']
  rest <- QC.listOf $ QC.elements identarr
  return $ h:rest

genNumber :: QC.Gen String
genNumber = QC.listOf1 $ QC.elements numbers

------------------------------------------------------------
-------------------- Helper Functions ----------------------
------------------------------------------------------------

getPosition :: Definition -> (Expr,Expr)
getPosition (Rectangle _ x y _ _ _) = (x,y) 
getPosition (Circle _ x y _ _) = (x,y)
getPosition _ = error "Trying to get position of something not a shape"

compareAnimations :: Animation -> Animation -> Bool
compareAnimations (views1,frames1) (views2,frames2) =
  let (b1,_) = g views1 views2
      b2 = f frames1 frames2
      f [] [] = True
      f [] _ = False
      f _ [] = False
      f (x:xs) (y:ys) = let (b,_) =g x y
                        in
                         b && f xs ys
      g x1 x2 = foldl (\(a,v2) v -> if a &&  v `elem` v2
                                   then (True,delete v v2)
                                   else (False,[])
                     ) (True,x2) x1
  in
   b1 && b2

isInEnvironment :: Ord k => k -> M.Map k a -> Bool
isInEnvironment id_ env = case M.lookup id_ env of
  Just _ -> True
  Nothing -> False

removeDuplex :: Eq a => [a] -> [a]
removeDuplex [] = []
removeDuplex (x:xs) = if x `elem` xs
                      then removeDuplex xs
                      else x:removeDuplex xs

----------- Copied from the SalsaInterp.hs file ------------

data Context = Context ConEnvironment State_
             deriving (Show)

type ConEnvironment = (Environment, [Ident], Integer)
type Environment = M.Map Ident Definition
type State_ = M.Map Ident [(Ident,Position)]


createEmptyContext :: Integer -> Context
createEmptyContext n = Context (M.empty, [], n) M.empty

lookupKey :: Ord a => a -> M.Map a b -> b
lookupKey key env =
  Mb.fromMaybe (error "Tried to look up unknown key ")
  (M.lookup key env)

getLast :: [a] -> ([a],[a])
getLast [] = ([],[])
getLast (x:[]) = ([],[x])
getLast (x:xs) = let (rest,last_) = getLast xs
                 in
                  (x:rest,last_)
