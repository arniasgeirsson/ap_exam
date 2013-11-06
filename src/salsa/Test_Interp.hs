module Test_Interp (runAllTests_i) where

import SalsaAst
import Gpx
import Test.HUnit
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Control.Monad
import Control.Exception
import SalsaInterp
import qualified Data.Map as M
import Data.Char
import Data.List

--runAllTests_i :: IO Bool
runAllTests_i = do
  putStrLn "Running tests.."
  runQCTest 50


identarr :: String
identarr = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

definitions :: [String]
definitions = ["viewdef","rectangle", "circle", "view" ]--, "group"]

commands :: [String]
commands = ["move", "at",--"par",
            "move","move","move"]

colours :: [(String,Colour)]
colours = [("blue",Blue), ("plum",Plum), ("red",Red),
           ("green",Green), ("orange",Orange)]

numbers :: String
numbers = ['0'..'9']

exprList :: [String]
exprList = ["const","plus", "minus", "const", "xproj", "yproj",
            "const","const","const","const"]

posList :: [String]
posList = ["abs","rel"]

------------------------ Test type -------------------------

newtype TestAnimation = TestAnimation ((Program,Integer), Animation)
                    deriving (Show, Eq)

instance QC.Arbitrary TestAnimation where
  arbitrary = do
    defcoms <- QC.listOf1 $ QC.elements $ definitions++commands
    (input,output) <- genManyDefcom ("viewdef":defcoms)
    return $ TestAnimation (input, output)

------------------------- Property -------------------------

prop_runProg :: TestAnimation -> Bool
prop_runProg (TestAnimation ((i,n),o)) = compareAnimations (runProg n i) o

---------------------- QC test runner ----------------------

-- TODO allow it to be user defined how many tests it must run
runQCTest :: Int -> IO ()
runQCTest n = QC.quickCheckWith QC.stdArgs{maxSuccess = n } prop_runProg

------------------------ Generators ------------------------

compareAnimations :: Animation -> Animation -> Bool
compareAnimations (views1,frames1) (views2,frames2) =
  let (b1,_) = foldl (\(a,v2) v -> if a &&  v `elem` v2
                                   then (True,delete v v2)
                                   else (False,[])
                     ) (True,views2) views1
      b2 = f frames1 frames2
      f [] [] = True
      f [] _ = False
      f _ [] = False
      f (x:xs) (y:ys) = let (b,_) = (foldl (\(a,y2) i -> if a &&  i `elem` y2
                                                         then (True,delete i y2)
                                                         else (False,[])
                                           ) (True,y) x)
                        in
                         b && f xs ys
  in
   b1 && b2


-- COM the framerate is intentionaly keept low to avoid very very big data sets
-- COM I assume that you cannot have group names in inside a group definition
-- COM I assume that no two names can be used to define two things

isInEnvironment id env = case M.lookup id env of
  Just _ -> True
  Nothing -> False

genManyDefcom :: [String] -> QC.Gen (([DefCom],Integer),Animation)
genManyDefcom [] = error "Cannot parse an empty list of definitions or commands"
genManyDefcom words_ = do
  n <- QC.elements ['1'..'9']
  let framerate = (read [n]::Integer)
      init = ((createEmptyContext framerate),[],([],[[]]))
--  init <- genDefcom "viewdef" (createEmptyContext framerate) ([],[[]])
  (_,all_defcoms,all_anim) <- foldM f init words_
  return ((all_defcoms,framerate),all_anim)
  where
    f (acontext,acci,anim) word = do
      (context,defcoms,new_anim) <- genDefcom word acontext anim
      case word `elem` commands && defcoms /= [] of
        False ->
          return (context,acci++defcoms,new_anim)
        True ->
          let (view,frames) = new_anim
              next = (reverse ([]:(reverse frames)))
          in
           return (context,acci++defcoms,(view,next))
genDefcom :: String -> Context -> Animation -> QC.Gen (Context,[DefCom],Animation)
genDefcom "viewdef" c@(Context (env,active,n) state) a@(views,frames) = do
  vident <- genVident
  case isInEnvironment vident env of
    True -> do
      return (c,[],a)
    False -> do
      expw <- genExpr env
      exph <- genExpr env
      w <- evalExpr_qc expw env
      h <- evalExpr_qc exph env
      let def = Viewdef vident expw exph
          env' = M.insert vident def env
      return ((Context (env',[vident],n) state),[Def def], ((views++[(vident,w,h)]),frames))
genDefcom "rectangle" c@(Context (env,active,n) state) a@(views,frames) = do
  sident <- genSident
  case isInEnvironment sident env of
    True -> do
      return (c,[],a)
    False -> do
      expx <- genExpr env
      expy <- genExpr env
      expw_ <- genExpr env
      expw <- forcePositive expw_ env
      exph_ <- genExpr env
      exph <- forcePositive exph_ env      
      x <- evalExpr_qc expx env
      y <- evalExpr_qc expy env
      w <- evalExpr_qc expw env
      h <- evalExpr_qc exph env
      (col,col_type) <- genColour
      let (rest,[last]) = getLast frames
          instrs = map (\viewName -> DrawRect x y w h viewName col) active
          def = Rectangle sident expx expy expw exph col_type
          env' = M.insert sident def env
          list = map (\viewName -> (viewName,(x,y))) active
          state' = M.insert sident list state
      return ((Context (env',active,n) state'),[Def def], (views,rest++[(last++instrs)]))
genDefcom "circle" c@(Context (env,active,n) state) a@(views,frames) = do
  sident <- genSident
  case isInEnvironment sident env of
    True -> do
      return (c,[],a)
    False -> do
      expx <- genExpr env
      expy <- genExpr env
      expr_ <- genExpr env
      expr <- forcePositive expr_ env
      x <- evalExpr_qc expx env
      y <- evalExpr_qc expy env
      r <- evalExpr_qc expr env
      (col,col_type) <- genColour
      let (rest,[last]) = getLast frames
          instrs = map (\viewName -> DrawCirc x y r viewName col) active
          def = Circle sident expx expy expr col_type
          env' = M.insert sident def env
          list = map (\viewName -> (viewName,(x,y))) active
          state' = M.insert sident list state
      return ((Context (env',active,n) state'),[Def def], (views,rest++[(last++instrs)]))
genDefcom "view" c@(Context (env,active,n) state) a@anim =
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
     (id,some_def) <- QC.elements flist
     let new_active = case some_def of
           (Group _ g) -> g
           (View v) -> [v]
     return ((Context (env,new_active,n) state), [Def $ View id],anim)
genDefcom "group" c@(Context (env,[],n) state) a@(views,frames) =
  return (c,[],a)
genDefcom "group" c@(Context (env,active,n) state) a@(views,frames) = do
  vident <- genVident
  case isInEnvironment vident env of
    True -> do
      return (c,[],a)
    False -> do
      new_actives_ <- QC.listOf1 $ QC.elements active
      let new_actives = removeDuplex new_actives_
          def = Group vident new_actives
          env' = M.insert vident def env
      return ((Context (env',new_actives,n) state), [Def def], a)
genDefcom "move" c@(Context (env,active,n) state) a@(views,frames) =
  let list = M.toList env
      flist = filter (\((x:_),def) -> isLower x) list
  in
   if null flist
   then return (c,[], a)
   else do
     ids <- QC.listOf1 $ QC.elements flist
     let ids2 = removeDuplex ids
     expPos <- genPos env
     (all_instr,new_state) <- foldM (f_ active expPos n env) ([],state) ids2
     let (rest,[last]) = getLast frames
         next1 = rest++[(last++all_instr)]
         ids_ = map (\(id,_) -> id) ids2
         def = Move ids_ expPos
     return ((Context (env,active,n) new_state),[Com def],(views,next1))
genDefcom "at" c@(Context (env,active,n) state) a@(views,frames) =
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
     (id,some_def) <- QC.elements flist
     let tmp_active = case some_def of
           (Group _ g) -> g
           (View v) -> [v]
     middle_cmd <- QC.elements commands
     middle <- genDefcom middle_cmd (Context (env,tmp_active,n) state) a
     let ((Context (env',_,_) state'), a_com, anim') = middle
     case a_com of
       [] ->
         return (c,[],a)
       [(Com some)] ->
         return ((Context (env',active,n) state'), [Com (At some id)], anim')
-- genDefcom "par" = do
--   word1 <- QC.elements commands
--   word2 <- QC.elements commands
--   (cmd1,Com cmdexp1:[]) <- genDefcom word1
--   (cmd2,Com cmdexp2:[]) <- genDefcom word2
--   input <- insertWhiteSpaces ["{",cmd1,"||",cmd2,"}"]
--   return (input, [Com $ Par cmdexp1 cmdexp2])
genDefcom s (Context (env,active,n) state) an = error $ "Cannot parse "++s++" into a DefCom"

forcePositive exp env = do
  val <- evalExpr_qc exp env
  if val >= 0 then
    return exp
    else
    return (Plus exp (Const (val*(-2))))

-- COM assumes that id is in state
f_ active pos n env acc (id,def) =
  foldM (\(acc_instrs, acc_state) a ->
           let positions = lookupKey id acc_state
           in
            case lookup a positions of
              Just old_pos -> do
                next_pos <- evalPos_ old_pos pos env
                let next_state = M.insert id (map (\(vn,p) -> if vn == a
                                                              then (vn,next_pos)
                                                              else (vn,p)
                                                  ) positions) acc_state
                instr <- gen_intrs old_pos next_pos def n a env
                return (acc_instrs++instr,next_state)
              Nothing ->
                return (acc_instrs,acc_state)) acc active

removeDuplex [] = []
removeDuplex (x:xs) = if x `elem` xs
                      then removeDuplex xs
                      else (x:removeDuplex xs)

gen_intrs opos npos (Rectangle _ _ _ ew eh ecol) n view env = do
  w <- evalExpr_qc ew env
  h <- evalExpr_qc eh env
  col <- evalColour_qc ecol
  let positions = interpolate n opos npos
  return $ map (\(x,y) -> DrawRect x y w h view col) positions
gen_intrs opos npos (Circle _ _ _ er ecol) n view env = do
  r <- evalExpr_qc er env
  col <- evalColour_qc ecol
  let positions = interpolate n opos npos
  return $ map (\(x,y) -> DrawCirc x y r view col) positions

evalPos_ _ (Abs expx expy) env = do
  x2 <- evalExpr_qc expx env
  y2 <- evalExpr_qc expy env
  return (x2,y2)
evalPos_ (x,y) (Rel expx expy) env = do
  x2 <- evalExpr_qc expx env
  y2 <- evalExpr_qc expy env
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

evalColour_qc :: Colour -> QC.Gen String
evalColour_qc Blue = return "blue"
evalColour_qc Plum = return "plum"
evalColour_qc Red = return "red"
evalColour_qc Green = return "green"
evalColour_qc Orange = return "orange"

evalExpr_qc :: Expr -> Environment -> QC.Gen Integer
evalExpr_qc (Const n) _ = return n
evalExpr_qc (Plus e1 e2) env = do
  n1 <- evalExpr_qc e1 env
  n2 <- evalExpr_qc e2 env
  return $ n1 + n2
evalExpr_qc (Minus e1 e2) env = do
  n1 <- evalExpr_qc e1 env
  n2 <- evalExpr_qc e2 env
  return $ n1 - n2
evalExpr_qc (Xproj id) env = do
  let def = lookupKey id env
      (expx,_) = getPosition def
  x <- evalExpr_qc expx env
  return x
evalExpr_qc (Yproj id) env = do
  let def = lookupKey id env
      (_,expy) = getPosition def
  y <- evalExpr_qc expy env
  return y

getPosition (Rectangle _ x y _ _ _) = (x,y) 
getPosition (Circle _ x y _ _) = (x,y)
getPosition _ = error "Trying to get position of something not a shape"

genExpr :: Environment -> QC.Gen (Expr)
genExpr env = do
  expr <- QC.elements exprList
  _genExpr expr env

-- COM/NOTE if i have to create a xproj or yproj, but no shape definition
  -- has been made yet, a Const will be returned instead.
_genExpr :: String -> Environment -> QC.Gen (Expr)
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
_genExpr "xproj" env = do
  let list = M.toList env
      flist = filter (\((x:_),def) -> isLower x) list
    in
   if null flist
   then
     _genExpr "const" env
   else do
     (sid,_) <- QC.elements flist
     return (Xproj sid)
_genExpr "yproj" env = do
  let list = M.toList env
      flist = filter (\((x:_),def) -> isLower x) list
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

-- getLast :: [a] -> ([a],[a])
-- getLast [] = ([],[])
-- getLast (x:[]) = ([],[x])
-- getLast (x:xs) = let (rest,last) = getLast xs
--                  in
--                   (x:rest,last)
