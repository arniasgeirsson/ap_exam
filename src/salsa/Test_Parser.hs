----------------------------------------------------------------------
--- Student name: Arni Asgeirsson
--- Student KU-id: lwf986
----------------------------------------------------------------------
module Test_Parser
       (runAllTests,runAllTestsWith)
       where

import Test.HUnit
import SalsaAst
import SalsaParser
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Control.Monad
import Control.Exception

------------------------------------------------------------
------------------ Interface to run tests ------------------
------------------------------------------------------------

runAllTests :: IO Bool
runAllTests = runAllTestsWith 100

runAllTestsWith :: Int -> IO Bool
runAllTestsWith n = do
  putStrLn "------------------------------------------------------------------"
  putStrLn "------------ Running tests for the SalsaParser module ------------"
  putStrLn "------------------------------------------------------------------\n"
  putStrLn "-------------------- Running QuickCheck tests --------------------"
  putStrLn $ "1. Testing if the parser parses the expected outputs from random"
    ++" valid input strings"
  putStrLn "Might take a few seconds ...\n"
  runQCTest n
  putStrLn "\n-------------------- Running HUnit tests -------------------------"
  putStrLn "2. Testing if the precedence and associativity works as intended\n"
  _ <- runTestTT precedenceCases
  putStrLn $ "\n3. Testing if the expected errors occur with specific invalid"
    ++" input strings\n"
  _ <- runTestTT errorCases
  putStrLn "\n-------------------- Running Unit tests --------------------------"
  putStrLn "4. Testing if an empty file parses as expected\n"
  b1 <- testF1
  print b1
  putStrLn "\n5. Testing if an simple Salsa file parses as expected\n"
  b2 <- testF2
  print b2
  putStrLn "\n6. Testing if an more advanced Salsa file parses as expected\n"
  b3 <- testF3
  print b3
  putStrLn "\n7. Testing if an invalid Salsa program from file parses as expected\n"
  b4 <- testF4
  print b4
  putStrLn "\n8. Testing if a non-existing file raises the expected error\n"
  testF5

------------------------------------------------------------
--------------------- QuickCheck Tests ---------------------
----------------- Test valid input strings -----------------
------------------------------------------------------------

----------------------- Definitions ------------------------
    
identarr :: String
identarr = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

definitions :: [String]
definitions = ["viewdef","rectangle", "circle", "view", "group"]

commands :: [String]
commands = ["move","at","par",
            "move","move","move"]

colours :: [(String,Colour)]
colours = [("blue",Blue), ("plum",Plum), ("red",Red),
           ("green",Green), ("orange",Orange)]

whiteSpaces :: [String]
whiteSpaces = [" ","\n"]

numbers :: String
numbers = ['0'..'9']

exprList :: [String]
exprList = ["plus", "minus", "const", "xproj", "yproj",
            "const","const","const","const"]

posList :: [String]
posList = ["abs","rel"]

------------------------ Test type -------------------------

newtype TestProgram = TestProgram (String, Either Error Program)
                    deriving (Show, Eq)

instance QC.Arbitrary TestProgram where
  arbitrary = do
    defcoms <- QC.listOf1 $ QC.elements $ definitions++commands
    (input,output_) <- genManyDefcom defcoms
    return $ TestProgram (input, Right output_)

------------------------- Property -------------------------

prop_pProgram :: TestProgram -> Bool
prop_pProgram (TestProgram (i,o)) = parseString i == o

---------------------- QC test runner ----------------------

runQCTest :: Int -> IO ()
runQCTest n = QC.quickCheckWith QC.stdArgs{maxSuccess = n } prop_pProgram

------------------------ Generators ------------------------

genManyDefcom :: [String] -> QC.Gen (String,[DefCom])
genManyDefcom [] = error "Cannot parse an empty list of definitions or commands"
genManyDefcom words_ = do
  result <- mapM genDefcom words_
  foldM f ("",[]) result
  where
    f (acci,acco) (i,o) = return (acci++i,acco++o)

genDefcom :: String -> QC.Gen (String, [DefCom])
genDefcom "viewdef" = do
  vident <- genVident
  (w,expw) <- genExpr
  (h,exph) <- genExpr
  input <- insertWhiteSpaces1 ["viewdef",vident,w,h]
  return (input, [Def $ Viewdef vident expw exph])
genDefcom "rectangle" = do
  sident <- genSident
  (x,expx) <- genExpr
  (y,expy) <- genExpr
  (w,expw) <- genExpr
  (h,exph) <- genExpr
  (col,col_type) <- genColour
  input <- insertWhiteSpaces1 ["rectangle",sident,x,y,w,h,col]
  return (input, [Def $ Rectangle sident expx expy expw exph col_type])
genDefcom "circle" = do
  sident <- genSident
  (x,expx) <- genExpr
  (y,expy) <- genExpr
  (r,expr) <- genExpr
  (col,col_type) <- genColour
  input <- insertWhiteSpaces1 ["circle",sident,x,y,r,col]
  return (input, [Def $ Circle sident expx expy expr col_type])
genDefcom "view" = do
  vident <- genVident
  input <- insertWhiteSpaces1 ["view",vident]
  return (input, [Def $ View vident])
genDefcom "group" = do
  vident <- genVident
  vidents <- QC.listOf1 genVident
  input <- insertWhiteSpaces1 $ ["group",vident,"["]++vidents++["]"]
  return (input, [Def $ Group vident vidents])
genDefcom "move" = do
  sidents <- QC.listOf1 genSident
  (pos,expPos) <- genPos
  input <- insertWhiteSpaces1 $ ["{"]++sidents++["->",pos,"}"]
  return (input, [Com $ Move sidents expPos])
genDefcom "at" = do
  word <- QC.elements commands
  (cmd,Com cmdexp:[]) <- genDefcom word
  vident <- genVident
  input <- insertWhiteSpaces1 ["{",cmd,"@",vident,"}"]
  return (input, [Com $ At cmdexp vident])
genDefcom "par" = do
  word1 <- QC.elements commands
  word2 <- QC.elements commands
  (cmd1,Com cmdexp1:[]) <- genDefcom word1
  (cmd2,Com cmdexp2:[]) <- genDefcom word2
  input <- insertWhiteSpaces ["{",cmd1,"||",cmd2,"}"]
  return (input, [Com $ Par cmdexp1 cmdexp2])
genDefcom s = error $ "Cannot parse "++s++" into a DefCom"

genPos :: QC.Gen (String, Pos)
genPos = do
  pos <- QC.elements posList
  _genPos pos

_genPos :: String -> QC.Gen (String, Pos)
_genPos "abs" = do
  (x,expx) <- genExpr
  (y,expy) <- genExpr
  input <- insertWhiteSpaces ["(",x,",",y,")"]
  return (input, Abs expx expy)
_genPos "rel" = do
  (x,expx) <- genExpr
  (y,expy) <- genExpr
  input <- insertWhiteSpaces ["+","(",x,",",y,")"]
  return (input, Rel expx expy)
_genPos s = error $ "Cannot parse "++s++" into an Pos"

genExpr :: QC.Gen (String, Expr)
genExpr = do
  expr <- QC.elements exprList
  _genExpr expr

_genExpr :: String -> QC.Gen (String, Expr)
_genExpr "plus" = do
  (e1,exp1) <- genExpr
  (e2,exp2) <- genExpr
  input <- insertWhiteSpaces ["(",e1,"+",e2,")"]
  return (input, Plus exp1 exp2)
_genExpr "minus" = do
  (e1,exp1) <- genExpr
  (e2,exp2) <- genExpr
  input <- insertWhiteSpaces ["(",e1,"-",e2,")"]
  return (input, Minus exp1 exp2)
_genExpr "const" = do
  n <- genNumber
  input <- insertWhiteSpaces ["(",n,")"]
  return (input, Const (read n::Integer))
_genExpr "xproj" = do
  sident <- genSident
  input <- insertWhiteSpaces ["(",sident,".","x",")"]
  return (input, Xproj sident)
_genExpr "yproj" = do
  sident <- genSident
  input <- insertWhiteSpaces ["(",sident,".","y",")"]
  return (input, Yproj sident)
_genExpr s = error $ "Cannot parse "++s++" into an Expr"

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

insertWhiteSpaces :: [String] -> QC.Gen String
insertWhiteSpaces = wsHelper QC.listOf

insertWhiteSpaces1 :: [String] -> QC.Gen String
insertWhiteSpaces1 = wsHelper QC.listOf1

wsHelper :: (QC.Gen String -> QC.Gen [String]) -> [String] -> QC.Gen String
wsHelper m words_ = do
  initWhite <- m $ QC.elements whiteSpaces
  foldM f (concat initWhite) words_
  where
    f acc word = do
      whitespaces <- QC.listOf1 $ QC.elements whiteSpaces
      return $ acc++word++concat whitespaces

------------------------------------------------------------
----------------------- HUnit tests ------------------------
--------------------- Precedence tests ---------------------
------------------------------------------------------------

precedenceCases :: Test
precedenceCases = TestLabel "Test cases for precedence"
                  $ TestList [testP1,testP2,testP3,testP4,testP5,
                              testP6,testP7,testP8,testP9,testP10,
                              testP11]

-- Show that @ and || are both left associative
testP1 :: Test
testP1 = let s = "a -> (0,0) @ A @ B @ C"
             a = "{{{ a -> (0,0) @ A } @ B } @ C }"
             d = "@ must be left associative"
         in TestCase $ assertEqual d (parseString a) (parseString s)
testP2 :: Test
testP2 = let s = "a -> (0,0) || b->(0,0) || c->(0,0)"
             a = "{{{a -> (0,0)} || b->(0,0)} || c->(0,0)}"
             d = "|| must be left associative"
         in TestCase $ assertEqual d (parseString a) (parseString s)

-- Left associativity of + and -
testP3 :: Test
testP3 = let s = "viewdef A 1 1+5+2"
             a = "viewdef A 1 (1+5)+2"
             d = "+ must be left associative"
         in TestCase $ assertEqual d (parseString a) (parseString s)
testP4 :: Test
testP4 = let s = "viewdef A 1 1-5-2"
             a = "viewdef A 1 (1-5)-2"
             d = "- must be left associative"
         in TestCase $ assertEqual d (parseString a) (parseString s)
testP5 :: Test
testP5 = let s = "viewdef A 1 1+5-2-5"
             a = "viewdef A 1 ((1+5)-2)-5"
             d = "+ & - must be left associative"
         in TestCase $ assertEqual d (parseString a) (parseString s)

-- Show that @ has higher precedence than ||
testP6 :: Test
testP6 = let s = "a -> (0,0) || b->(0,0) @ A"
             a = "a -> (0,0) || {b->(0,0) @ A}"
             d = "@ should have higher precedence that ||"
         in TestCase $ assertEqual d (parseString a) (parseString s)
testP7 :: Test
testP7 = let s = "a -> (0,0) || b->(0,0) @ A || c->(0,0) @ B @ C"
             a = "a -> (0,0) || {b->(0,0) @ A} || {{c->(0,0) @ B} @ C}"
             d = "@ should have higher precedence that ||"
         in TestCase $ assertEqual d (parseString a) (parseString s)

-- Show that + and - has the same precedence (ie it maintains its order)
testP8 :: Test
testP8 = let s = "viewdef A 1 1+5-4"
             a = "viewdef A 1 ((1+5)-4)"
             d = "+ & - should have the same precedence"
         in TestCase $ assertEqual d (parseString a) (parseString s)
testP9 :: Test
testP9 = let s = "viewdef A 1 1-5+4"
             a = "viewdef A 1 ((1-5)+4)"
             d = "+ & - should have the same precedence"
         in TestCase $ assertEqual d (parseString a) (parseString s)

-- Show that . has higher precedence than + and -
testP10 :: Test
testP10 = let s = "viewdef A 1 r.x + c.y"
              a = "viewdef A 1 (r.x) + (c.y)"
              d = ". should have higher precedence than +"
          in TestCase $ assertEqual d (parseString a) (parseString s)
testP11 :: Test
testP11 = let s = "viewdef A 1 r.x - c.y"
              a = "viewdef A 1 (r.x) - (c.y)"
              d = ". should have higher precedence than -"
          in TestCase $ assertEqual d (parseString a) (parseString s)

------------------------------------------------------------
----------------------- Error tests ------------------------
------------------------------------------------------------
errorCases :: Test
errorCases = TestLabel "Test cases for invalid input strings"
             $ TestList [testE1, testE2, testE3, testE4, testE5,
                         testE6, testE7, testE8, testE9, testE10,
                         testE11,testE12,testE13,testE14, testE15,
                         testE16,testE17,testE18,testE19,testE20,
                         testE21,testE22,testE23,testE24,testE25,
                         testE26,testE27,testE28,testE29,testE30,
                         testE31,testE32,testE33,testE34,testE35,
                         testE36,testE37,testE38,testE39,testE40,
                         testE41]

-- Parse empty string
testE1 :: Test
testE1 = let s = ""
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Using sident in place of vident
testE2 :: Test
testE2 = let s = "viewdef sident 4 5"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE3 :: Test
testE3 = let s = "view sident"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE4 :: Test
testE4 = let s = "group sident [A,B,C]"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE5 :: Test
testE5 = let s = "group Vident [A,B,sident]"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE6 :: Test
testE6 = let s = "{ a -> (1,1) } @ sident"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Using vident in place of sident
testE7 :: Test
testE7 = let s = "rectangle Vident 1 2 3 4 green"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE8 :: Test
testE8 = let s = "circle Vident 1 2 3 red"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE9 :: Test
testE9 = let s = "{ Vident -> (1,1) } @ A"
         in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE10 :: Test
testE10 = let s = "{ a b c Vident -> (1,1) } @ A"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE11 :: Test
testE11 = let s = "viewdef A 1 (1 + Vident . x)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Using wrong types of characters
testE12 :: Test
testE12 = let s = "viewdef _A 1 2"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE13 :: Test
testE13 = let s = "viewdef A+a 1 2"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE14 :: Test
testE14 = let s = "view Bæ"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE15 :: Test
testE15 = let s = "circle a 1 2 (a . z) red"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Using numbers instead of letters, (same as using wrong characters?)
testE16 :: Test
testE16 = let s = "5 -> (0,0)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE17 :: Test
testE17 = let s = "group 2 [ A ]"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Using letters instead of numbers
testE18 :: Test
testE18 = let s = "viewdef A b 4"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE19 :: Test
testE19 = let s = "viewdef A 4 (a+2)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- TODO does it make sense to test for this?
-- Using wrong parenthesis
testE20 :: Test
testE20 = let s = "( a -> (0,0) )"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE21 :: Test
testE21 = let s = "group A { B }"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE22 :: Test
testE22 = let s = "{ a -> [0,0] }"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- TODO does it make sense to test for this?
-- Using non-existing operator
testE23 :: Test
testE23 = let s = "a -> - (1,2)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE24 :: Test
testE24 = let s = "viewdef A 1 (2 * 4)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE25 :: Test
testE25 = let s = "circle a 3 1 ( a , x) red"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Using a reserved word as sident (res+colour)
testE26 :: Test
testE26 = let s = "rectangle viewdef 1 2 3 4 green"
            in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE27 :: Test
testE27 = let s = "rectangle rectangle 1 2 3 4 green"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE28 :: Test
testE28 = let s = "circle a 1 2 (1 + group . x)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE29 :: Test
testE29 = let s = "green -> (6,66)"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- TODO does it make sense to test for this?
-- Using invalid colour name
testE30 :: Test
testE30 = let s = "circle a 1 2 3 purple"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Wrong whitespace inbetween stuff
testE31 :: Test
testE31 = let s = "circle a 31 ( a , x) red"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE32 :: Test
testE32 = let s = "circlea 3 1 ( a , x) red"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE33 :: Test
testE33 = let s = "viewdefA 1 2"
          in TestCase $ assertEqual ""
             (Right [Def $ Viewdef "A" (Const 1) (Const 2)])
             (parseString s)
testE34 :: Test
testE34 = let s = "group A [BCD]"
          in TestCase $ assertEqual ""
             (Right [Def $ Group "A" ["BCD"]])
             (parseString s)

-- Grouping zero vidents
testE35 :: Test
testE35 = let s = "group A [ ]"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Show that the salsa is case sensitive
testE36 :: Test
testE36 = let s = "Viewdef A 1 2"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE37 :: Test
testE37 = let s = "Group A [ B ]"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE38 :: Test
testE38 = let s = "rectangle a 1 2 3 4 Orange"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Use negative values
testE39 :: Test
testE39 = let s = "viewdef A -2 5"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)
testE40 :: Test
testE40 = let s = "viewdef A (-2) 5"
          in TestCase $ assertEqual "" (Left $ NoParsePossible s) (parseString s)

-- Show that simple expression can create negative values to by pass the
-- integer restriction
testE41 :: Test
testE41 = let s = "viewdef A (0-2) 5"
          in TestCase $ assertEqual ""
             (Right [Def $ Viewdef "A" (Minus (Const 0) (Const 2)) (Const 5)])
             (parseString s)

-- Show the UnexpectedRemainder error
-- Umm how?

------------------------------------------------------------
------------------------ Unit tests ------------------------
--------------------- Parse file tests ---------------------
------------------------------------------------------------

-- Parse empty file
testF1 :: IO Bool
testF1 = testFile "test_files/empty.salsa"

-- Parse simple salsa
testF2 :: IO Bool
testF2 = testFile "test_files/simple.salsa"

-- Parse multi salsa
testF3 :: IO Bool
testF3 = testFile "test_files/multi.salsa"

-- Parse invalid salsa
testF4 :: IO Bool
testF4 = testFile "test_files/invalid.salsa"

-- Parse non-existing file
-- Note that if the file does exist then it returns the parse from there
-- So this does only test the intended if the file indeed does not exist.
testF5 :: IO Bool
testF5 = catch (testFile "test_files/doesNotExist.salsa")
         (\e -> do let _ = e::IOException
                   return True)

-- A helper function to compare the results from parseString and parseFile
testFile :: String -> IO Bool
testFile path_ = do
  content <- readFile path_
  output_ <- parseFile path_
  return $ parseString content == output_
