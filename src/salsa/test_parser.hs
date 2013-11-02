module Test_Parser where

import Test.HUnit
import SalsaAst
import SalsaParser
import qualified Test.QuickCheck as QC
import Control.Monad

main = runTestTT $ TestList [definitionCases]

definitionCases = TestLabel "Test cases for definitions" (TestList [test_viewdef])

test_viewdef = TestCase $ assertEqual "Should return a ViewDef definition"
               (Right [(Def $ Viewdef "View" (Const 20) (Const 40))])
               (parseString "viewdef View 20 40")


-- TODO remove useless unit test above
-- TODO add unit tests for precedence
-- TODO add unit tests for error prone parsings
-- TODO add unit tests for parseFile

newtype TestProgram = TestProgram (String, Either Error Program)
                    deriving (Show, Eq)

identarr :: String
identarr = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

definitions :: [String]
definitions = ["viewdef","rectangle", "circle", "view", "group"]

commands :: [String]
commands = ["move","move","move","move","at","par"]

colours :: [(String,Colour)]
colours = [("blue",Blue), ("plum",Plum), ("red",Red), ("green",Green), ("orange",Orange)]

-- TODO add all types of whiteSpace
whiteSpaces :: [String]
whiteSpaces = [" "]

numbers :: String
numbers = ['0'..'9']

expr_list = ["plus", "minus", "const","const","const","const","const", "xproj", "yproj"]


instance QC.Arbitrary TestProgram where
  arbitrary = do
    defcoms <- QC.listOf1 $ QC.elements $ definitions++commands
    (input,output) <- gen_many_defcom defcoms
    return $ TestProgram (input, Right output)

-- TODO optimize it, it is very slow
prop_pProgram :: TestProgram -> Bool
prop_pProgram (TestProgram (i,o)) = (parseString i) == o

runQCTests1 :: IO ()
runQCTests1 = do QC.quickCheck prop_pProgram

gen_many_defcom :: [String] -> QC.Gen (String,[DefCom])
gen_many_defcom words = do
  result <- mapM gen_defcom words
  foldM f ("",[]) result
  where
    f (acci,acco) (i,o) = return $ (acci++i,acco++o)

-- TODO there is error when removing the parenthesis {}
gen_defcom :: String -> QC.Gen (String, [DefCom])
gen_defcom "viewdef" = do
  vident <- gen_vident
  (w,expw) <- gen_expr
  (h,exph) <- gen_expr
  input <- insert_whiteSpaces1 ["viewdef",vident,w,h]
  return (input, [Def $ Viewdef vident expw exph])
gen_defcom "rectangle" = do
  sident <- gen_sident
  (x,expx) <- gen_expr
  (y,expy) <- gen_expr
  (w,expw) <- gen_expr
  (h,exph) <- gen_expr
  (col,col_type) <- gen_colour
  input <- insert_whiteSpaces1 ["rectangle",sident,x,y,w,h,col]
  return (input, [Def $ Rectangle sident expx expy expw exph col_type])
gen_defcom "circle" = do
  sident <- gen_sident
  (x,expx) <- gen_expr
  (y,expy) <- gen_expr
  (r,expr) <- gen_expr
  (col,col_type) <- gen_colour
  input <- insert_whiteSpaces1 ["circle",sident,x,y,r,col]
  return (input, [Def $ Circle sident expx expy expr col_type])
gen_defcom "view" = do
  vident <- gen_vident
  input <- insert_whiteSpaces1 ["view",vident]
  return (input, [Def $ View vident])
gen_defcom "group" = do
  vident <- gen_vident
  vidents <- QC.listOf1 gen_vident
  input <- insert_whiteSpaces1 $ ["group",vident,"["]++vidents++["]"]
  return (input, [Def $ Group vident vidents])
gen_defcom "move" = do
  sidents <- QC.listOf1 gen_sident
  (pos,expPos) <- gen_pos
  input <- insert_whiteSpaces1 $ ["{"]++sidents++["->",pos,"}"]
  return (input, [Com $ Move sidents expPos])
gen_defcom "at" = do
  word <- QC.elements commands
  (cmd,Com cmdexp:[]) <- gen_defcom word
  vident <- gen_vident
  input <- insert_whiteSpaces1 ["{",cmd,"@",vident,"}"]
  return (input, [Com $ At cmdexp vident])
gen_defcom "par" = do
  word1 <- QC.elements commands
  word2 <- QC.elements commands
  (cmd1,Com cmdexp1:[]) <- gen_defcom word1
  (cmd2,Com cmdexp2:[]) <- gen_defcom word2
  input <- insert_whiteSpaces ["{",cmd1,"||",cmd2,"}"]
  return (input, [Com $ Par cmdexp1 cmdexp2])

pos_list = ["abs","rel"]

gen_pos :: QC.Gen (String, Pos)
gen_pos = do
  pos <- QC.elements pos_list
  gen_pos1 pos

gen_pos1 :: String -> QC.Gen (String, Pos)
gen_pos1 "abs" = do
  (x,expx) <- gen_expr
  (y,expy) <- gen_expr
  input <- insert_whiteSpaces ["(",x,",",y,")"]
  return (input, Abs expx expy)
gen_pos1 "rel" = do
  (x,expx) <- gen_expr
  (y,expy) <- gen_expr
  input <- insert_whiteSpaces ["+","(",x,",",y,")"]
  return (input, Rel expx expy)

gen_expr :: QC.Gen (String, Expr)
gen_expr = do
  expr <- QC.elements expr_list
  gen_expr1 expr

-- TODO there is error when removing the parenthesis ()
gen_expr1 :: String -> QC.Gen (String, Expr)
gen_expr1 "plus" = do
  (e1,exp1) <- gen_expr
  (e2,exp2) <- gen_expr
  input <- insert_whiteSpaces ["(",e1,"+",e2,")"]
  return (input, Plus exp1 exp2)
gen_expr1 "minus" = do
  (e1,exp1) <- gen_expr
  (e2,exp2) <- gen_expr
  input <- insert_whiteSpaces ["(",e1,"-",e2,")"]
  return (input, Minus exp1 exp2)
gen_expr1 "const" = do
  n <- gen_number
  input <- insert_whiteSpaces ["(",n,")"]
  return (input, Const (read n::Integer))
gen_expr1 "xproj" = do
  sident <- gen_sident
  input <- insert_whiteSpaces ["(",sident,".","x",")"]
  return (input, Xproj sident)
gen_expr1 "yproj" = do
  sident <- gen_sident
  input <- insert_whiteSpaces ["(",sident,".","y",")"]
  return (input, Yproj sident)
  
gen_colour :: QC.Gen (String,Colour)
gen_colour = QC.elements colours

gen_vident :: QC.Gen String
gen_vident = do
  h <- QC.elements ['A'..'Z']
  rest <- QC.listOf $ QC.elements identarr
  return $ h:rest

gen_sident :: QC.Gen String
gen_sident = do
  h <- QC.elements ['a'..'z']
  rest <- QC.listOf $ QC.elements identarr
  return $ h:rest

gen_number :: QC.Gen String
gen_number = QC.listOf1 $ QC.elements numbers

insert_whiteSpaces :: [String] -> QC.Gen String
insert_whiteSpaces = ws_helper QC.listOf

insert_whiteSpaces1 :: [String] -> QC.Gen String
insert_whiteSpaces1 = ws_helper QC.listOf1

ws_helper :: (QC.Gen String -> QC.Gen [String]) -> [String] -> QC.Gen String
ws_helper m words = do
  initWhite <- m $ QC.elements whiteSpaces
  foldM f (concat initWhite) words
  where
    f acc word = do
      whiteSpaces <- QC.listOf1 $ QC.elements whiteSpaces
      return $ acc++word++(concat whiteSpaces)
