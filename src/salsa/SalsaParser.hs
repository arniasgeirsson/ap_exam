module SalsaParser (parseString, parseFile, Error (..)) where

import SalsaAst
import Text.ParserCombinators.ReadP
import qualified Test.QuickCheck as QC
import Test.HUnit
import Control.Exception
import Control.Monad

-- ?
runParser :: ReadP Program
runParser = do
  skipSpaces
  p <- pProgram
  skipSpaces
  eof
  return p

-- Parser functions

pProgram :: ReadP Program
pProgram = pDefComs

pDefComs :: ReadP [DefCom]
pDefComs = do
  dc <- pDefCom
  dcs <- pDefComs'
  return $ dc:dcs

-- why not <++?
pDefComs' :: ReadP [DefCom]
pDefComs' =
  pDefComs
  +++
  return []

pDefCom :: ReadP DefCom
pDefCom = do
  cmd <- pCommand
  return $ Com cmd
-- why not <++?
  +++
  do
    def <- pDefinition
    return $ Def def

-- TODO split into smaller parses ?
-- Would also stop hlint to mention a hint
pDefinition :: ReadP Definition
pDefinition = do
  stringT "viewdef"
  vid <- pVIdent
  exp1 <- pExpr
  exp2 <- pExpr
  return $ Viewdef vid exp1 exp2
  +++
  do
    stringT "rectangle"
    sid <- pSIdent
    exp1 <- pExpr
    exp2 <- pExpr    
    exp3 <- pExpr
    exp4 <- pExpr
    col <- pColour
    return $ Rectangle sid exp1 exp2 exp3 exp4 col
  +++
  do
    stringT "circle"
    sid <- pSIdent
    exp1 <- pExpr
    exp2 <- pExpr
    exp3 <- pExpr
    col <- pColour
    return $ Circle sid exp1 exp2 exp3 col
  +++
  do
    stringT "view"
    vid <- pVIdent
    return $ View vid
  +++
  do
    stringT "group"
    vid <- pVIdent
    vids <- bracks '[' pVIdents ']'
    return $ Group vid vids

pCommand :: ReadP Command
pCommand = do
  cmd2 <- pCommand2
  pCommand' cmd2

pCommand' :: Command -> ReadP Command
pCommand' iV = do
  stringT "||"
  cmd2 <- pCommand2
  pCommand' $ Par iV cmd2
  +++
  return iV

pCommand2 :: ReadP Command
pCommand2 = do
  cmd3 <- pCommand3
  pCommand2' cmd3

pCommand2' :: Command -> ReadP Command
pCommand2' iV = do
  charT '@'
  vid <- pVIdent
  pCommand2' $ At iV vid
  +++
  return iV

pCommand3 :: ReadP Command
pCommand3 = do
  sids <- pSIdents
  stringT "->"
  pos <- pPos
  return $ Move sids pos
  +++
  bracks '{' pCommand '}'




-- pCommand :: ReadP Command
-- pCommand = do
--   sids <- pSIdents
--   stringT "->"
--   pos <- pPos
--   pCommand' $ Move sids pos
--   +++
--   do
--     cmd <- bracks '{' pCommand '}'
--     pCommand' cmd

-- pCommand' :: Command -> ReadP Command
-- pCommand' iV = do
--   cmd <- pRest iV
--   pCommand' cmd
--   +++
--   return iV

-- pRest :: Command -> ReadP Command
-- pRest iV = do
--   charT '@'
--   vid <- pVIdent
--   return $ At iV vid
--   +++
--   do
--     stringT "||"
--     cmd <- pCommand
--     return $ Par iV cmd

pVIdents :: ReadP [Ident]
pVIdents = do
  vid <- pVIdent
  vids' <- pVIdents'
  return $ vid:vids'

pVIdents' :: ReadP [Ident]
pVIdents' =
  pVIdents
  +++
  return []

pSIdents :: ReadP [Ident]
pSIdents = do
  sid <- pSIdent
  sids' <- pSIdents'
  return $ sid:sids'

pSIdents' :: ReadP [Ident]
pSIdents' =
  pSIdents
  +++
  return []

pPos :: ReadP Pos
pPos =
  bracks '(' (pMiddle Abs) ')'
  +++
  do
  charT '+'
  bracks '(' (pMiddle Rel) ')'

-- Helper
pMiddle :: (Expr -> Expr -> b) -> ReadP b
pMiddle c = do
  exp1 <- pExpr
  charT ','
  exp2 <- pExpr
  return $ c exp1 exp2


pExpr :: ReadP Expr
pExpr = do
  prim <- pPrim
  pExpr' prim

pExpr' :: Expr -> ReadP Expr
pExpr' iV = do
  exp_ <- pRest2 iV
  pExpr' exp_
  +++
  return iV

pRest2 :: Expr -> ReadP Expr
pRest2 iV = do
  charT '+'
  prim <- pPrim
  return $ Plus iV prim
  +++
  do
  charT '-'
  prim <- pPrim
  return $ Minus iV prim

pPrim :: ReadP Expr
pPrim = do
  int <- pInteger
  return $ Const int
  +++
  bracks '(' pExpr ')'
  +++
  do
  sid <- pSIdent
  charT '.'
  pRest3 sid

pRest3 :: Ident -> ReadP Expr
pRest3 iV = do
  charT 'x'
  return $ Xproj iV
  +++
  do
  charT 'y'
  return $ Yproj iV

pColour :: ReadP Colour
pColour = do
  stringT "blue"
  return Blue
  +++
  do
  stringT "plum"
  return Plum
  +++
  do
  stringT "red"
  return Red
  +++
  do
  stringT "green"
  return Green
  +++
  do
  stringT "orange"
  return Orange


-- Not described by grammar
identarr :: String
identarr = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

reservedWords :: [String]
reservedWords = ["viewdef","rectangle", "circle", "group", "view"]

colourNames :: [String]
colourNames = ["blue", "plum", "red", "green", "orange"]

pVIdent :: ReadP Ident
pVIdent = do
  skipSpaces
  h <- satisfy (`elem` ['A'..'Z'])
  rest <- munch (`elem` identarr)
  skipSpaces
  return $ h:rest

pSIdent :: ReadP Ident
pSIdent =  do
  skipSpaces
  h <- satisfy (`elem` ['a'..'z'])
  rest <- munch (`elem` identarr)
  skipSpaces
  let ident = h:rest
  if ident `elem` (reservedWords++colourNames)
    then pfail
    else return ident
  
pInteger :: ReadP Integer
pInteger = do
  skipSpaces
  n <- munch1 (`elem` ['0'..'9'])
  skipSpaces
  -- Note: read is a partial function
  return (read n::Integer)

-- Small parser helpers
bracks ::  Char -> ReadP b -> Char -> ReadP b
bracks lb a rb = do
  charT lb
  b <- a
  charT rb
  return b

stringT :: String -> ReadP ()
stringT s = do
  skipSpaces
  _ <- string s
  skipSpaces

charT :: Char -> ReadP ()
charT c = do
  skipSpaces
  _ <- char c
  skipSpaces

-- Interface

data Error = NoParsePossible String
           | AmbiguousGrammar [(Program, String)]
           | UnexpectedRemainder Program String
           deriving (Eq, Show)

--parse :: ReadP a -> String -> Either Error a
parse :: ReadP Program -> String -> Either Error Program
parse parser s =
  case readP_to_S parser s of
    [(result, "")] -> Right result
    [(result, unparsed)] -> Left $ UnexpectedRemainder result unparsed
    [] -> Left $ NoParsePossible s
    results -> Left $ AmbiguousGrammar results

parseString :: String -> Either Error Program
parseString = parse runParser

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = do
  content <- readFile filename
  return $ parseString content


---------- Tests ----------

-- Test charT parser

--prop_charT :: Char -> Bool
--prop_charT c = (readP_to_S charT [c]) == [("","")]

-- parseQC :: ReadP a -> String -> Either String a
-- parseQC parser s =
--   case readP_to_S parser s of
--     [(result, "")] -> Right result
--     _ -> Left "Wrong.."

-- newtype TestInteger = TestInteger (String, Either String Integer)
--                     deriving(Eq, Show)

-- instance QC.Arbitrary TestInteger where
--   arbitrary = do
--     n <- QC.listOf1 $ QC.elements ['0'..'9']
--     return $ TestInteger (n,Right (read n::Integer)) 

-- prop_pInteger :: TestInteger -> Bool
-- prop_pInteger (TestInteger (i,o)) =
--   (parseQC pInteger i) == o

-- -- prop_pBracks -> c1 parser s c2 == parser s

-- runQCTests :: IO ()
-- runQCTests = do
--   QC.quickCheck prop_pInteger


-- runHUTests = runTestTT $ TestList [case_bracks]

-- case_bracks = TestLabel "bracks test cases" $ TestList [test_bracks_Empty]

-- test_bracks_Empty = TestCase $ assertEqual "" (Left $ NoParsePossible ) (parseString "( )") 
