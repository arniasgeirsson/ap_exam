module SalsaParser (parseString, parseFile, Error (..)) where

import SalsaAst
import Text.ParserCombinators.ReadP

------------------------------------------------------------
------------------ The interface ---------------------------
------------------------------------------------------------

data Error = NoParsePossible String
           | AmbiguousGrammar [(Program, String)]
           | UnexpectedRemainder Program String
           deriving (Eq, Show)

parseString :: String -> Either Error Program
parseString = parse runParser

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = do
  content <- readFile filename
  return $ parseString content

------------------------------------------------------------
------------------- The parser functions -------------------
------------------------------------------------------------

----------------------- Definitions ------------------------

identarr :: String
identarr = ['A'..'Z']++['a'..'z']++['0'..'9']++"_"

reservedWords :: [String]
reservedWords = ["viewdef","rectangle", "circle", "group", "view"]

colourNames :: [String]
colourNames = ["blue", "plum", "red", "green", "orange"]

--------------------- Top-Level parsers --------------------

parse :: ReadP Program -> String -> Either Error Program
parse parser s =
  case readP_to_S parser s of
    [(result, "")] -> Right result
    [(result, unparsed)] -> Left $ UnexpectedRemainder result unparsed
    [] -> Left $ NoParsePossible s
    results -> Left $ AmbiguousGrammar results

runParser :: ReadP Program
runParser = do
  skipSpaces
  p <- pProgram
  skipSpaces
  eof
  return p

--------------------- Grammar Parsers ----------------------

pProgram :: ReadP Program
pProgram = pDefComs

pDefComs :: ReadP [DefCom]
pDefComs = do
  dc <- pDefCom
  dcs <- pDefComs'
  return $ dc:dcs

-- why not <++?
pDefComs' :: ReadP [DefCom]
pDefComs' =  pDefComs +++ return []

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
pDefinition = hViewdef +++ hRectangle +++ hCircle +++ hView +++ hGroup

hViewdef :: ReadP Definition
hViewdef = do
  stringT "viewdef"
  vid <- pVIdent
  exp1 <- pExpr
  exp2 <- pExpr
  return $ Viewdef vid exp1 exp2

hRectangle :: ReadP Definition
hRectangle = do
  stringT "rectangle"
  sid <- pSIdent
  exp1 <- pExpr
  exp2 <- pExpr
  exp3 <- pExpr
  exp4 <- pExpr
  col <- pColour
  return $ Rectangle sid exp1 exp2 exp3 exp4 col

hCircle :: ReadP Definition
hCircle = do
  stringT "circle"
  sid <- pSIdent
  exp1 <- pExpr
  exp2 <- pExpr
  exp3 <- pExpr
  col <- pColour
  return $ Circle sid exp1 exp2 exp3 col

hView :: ReadP Definition
hView = do
  stringT "view"
  vid <- pVIdent
  return $ View vid

hGroup :: ReadP Definition
hGroup = do
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
pSIdents' = pSIdents +++ return []

pPos :: ReadP Pos
pPos =
  bracks '(' (hMiddle Abs) ')'
  +++
  do
  charT '+'
  bracks '(' (hMiddle Rel) ')'

hMiddle :: (Expr -> Expr -> b) -> ReadP b
hMiddle c = do
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
  exp_ <- pOp iV
  pExpr' exp_
  +++
  return iV

pOp :: Expr -> ReadP Expr
pOp iV = do
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
  pProj sid

pProj :: Ident -> ReadP Expr
pProj iV = do
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

----------------------- Extra parsers ----------------------

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

---------------------- Helper parsers ----------------------

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
