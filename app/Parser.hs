module Parser where

import           BaseType
import           Control.Monad.Combinators.Expr (makeExprParser)
import           Data.Functor                   (void, ($>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Void
import           Debug.Trace
import           Expr                           (Expr (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char           (alphaNumChar, eol, letterChar,
                                                 newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Char.Lexer     (space)
import Text.Megaparsec.Debug (dbg)

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

reservedWords :: [String]
reservedWords = ["define",
                 "let",
                 "function",
                 "Type",
                 "where",
                 "match",
                 "with",
                 "list",
                 "map",
                 "True",
                 "False",
                 "data"]

reservedWord :: String -> Parser ()
reservedWord w = do
    (lexeme . try) $ do
        _ <- string w
        notFollowedBy alphaNumChar
        return ()

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

parseSubtype :: Parser Expr
parseSubtype = do
    _ <- reservedWord "define"
    t1 <- parseExpr
    _ <- symbol "<:"
    t2 <- parseVariable
    return $ Subtype t1 t2

parseDefinition :: Parser Expr
parseDefinition = do
    _ <- reservedWord "define"
    id <- identifier
    _ <- symbol ":"
    ty <- parseUniverse <|> parseVariable
    return $ Definition id ty

parseLet :: Parser Expr
parseLet =  do
    reservedWord "let"
    x <- identifier
    _ <- symbol ":"
    a <- try parseType
    _ <- symbol "="
    t <- parseExpr
    u <- parseExpr
    pure $ Let x a t u

parseLetDefinition :: Parser Expr
parseLetDefinition = do
    reservedWord "let"
    x <- identifier
    args <- many identifier
    _ <- symbol ":"
    a <- try parseType
    _ <- symbol "="
    t <- parseExpr
    pure $ Let x a t (Var x)

pBind :: Parser String
pBind = identifier <|> symbol "_"

parseVariable :: Parser Expr
parseVariable = Var <$> identifier

parseSpine :: Parser Expr
parseSpine = foldl1 App <$> some parseAtom

parseLambda :: Parser Expr
parseLambda = do
    symbol "\\"
    var <- identifier
    _ <- symbol ":"
    varType <- parseAtom
    _ <- symbol "."
    body <- parseExpr
    return $ Lambda var varType body

parseUniverse :: Parser Expr
parseUniverse = do
    _ <- reservedWord "Type"
    Universe <$> L.decimal

parseType :: Parser Expr
parseType = parseArrowType

parseArrowType :: Parser Expr
parseArrowType = do
    t1 <- parseTypeAtom
    option t1 $ do
        _ <- symbol "->"
        t2 <- parseArrowType
        let freshVar = "_x"
        return $ Pi freshVar t1 t2

parseTypeAtom :: Parser Expr
parseTypeAtom = try parseListType <|> parseUniverse <|> parseVariable <|> between (symbol "(") (symbol ")") parseType

parseStatement :: Parser Expr
parseStatement = dbg "Debug statement" $ try parseMatchStatement <|> try parseInductive <|> try parseDefinition <|> try parseSubtype <|> try parseLetDefinition <|> try parseFunctionDeclarationWithImplementation
    <|> try parseExpr

statementSeparator :: Parser ()
statementSeparator = choice
  [ void (symbol ";")
  , void (symbol ".")
  , skipSome (try eol)
  ]
parseProgram :: Parser [Expr]
parseProgram = spaceConsumer *> sepEndBy parseStatement statementSeparator <* eof

parseExpr :: Parser Expr
parseExpr = parseTerm <|> parseMap

parseTerm :: Parser Expr
parseTerm = do
    first <- try parseLet <|> try parseLambda <|> parseSimpleTerm

    option first $ do
        _ <- symbol "+"
        BinOp first <$> parseTerm

parseSimpleTerm :: Parser Expr
parseSimpleTerm = try parseSpine <|> parseAtom

parseBinOp :: Parser Expr
parseBinOp = do
    a <- parseAtom <|> parseSpine
    _ <- symbol "+" -- TODO: support for /,-,*
    b <- parseExpr
    return $ BinOp a b

parseAtom :: Parser Expr
parseAtom = choice
    [ parseVariable
    , parseUniverse
    , parseList
    , integer
    , boolean
    , parseMatchStatement
    , between (symbol "(") (symbol ")") parseExpr
    ]

parseInductive :: Parser Expr
parseInductive = do
    reservedWord "data"
    name <- identifier
    _ <- symbol ":"
    t <- parseAtom
    reservedWord "where"
    matchBranches <- manyTill parseMatchBranches (lookAhead (void (symbol ".") <|> void parseStatementStart))
    return $ Inductive name t matchBranches

parseStatementStart :: Parser ()
parseStatementStart = choice
    [ void (reservedWord "define")
    , void (reservedWord "let")
    , void (reservedWord "match")
    , void (reservedWord "data")
    ]

parseMatchBranches :: Parser (String, Expr)
parseMatchBranches = do
    _ <- symbol "|"
    id <- identifier
    _ <- symbol ":"
    t <- try parseArrowType <|> parseAtom
    return $ (id, t)

integer :: Parser Expr
integer = do
    n <- lexeme L.decimal
    return (BaseType (Integer n))

boolean :: Parser Expr
boolean = do
    value <- choice [reservedWord "True" *> pure True, reservedWord "False" *> pure False]
    return (BaseType (Boolean value))

parseListType :: Parser Expr
parseListType  = do
    elementType <- parseAtom
    _ <- reservedWord "list"
    return $ App (Var "list") elementType

parseList :: Parser Expr
parseList = do
    _ <- symbol "["
    elements <- sepBy parseExpr (symbol ",")
    _ <- symbol "]"

    if null elements
        then return $ List (Var "Dummy") []
        else return $ List (Var "Dummy") elements


parseMap :: Parser Expr
parseMap = do
    _ <- reservedWord "map"
    f <- parseAtom
    xs <- parseAtom
    return $ Map f xs

parseFunctionDeclaration :: Parser (String, Expr)
parseFunctionDeclaration = do
    _ <- reservedWord "let"
    name <- identifier
    _ <- symbol ":"
    t <- parseType
    pure (name, t)

parseFunctionDeclarationWithBody :: Parser (String, Expr)
parseFunctionDeclarationWithBody = do
    name <- identifier
    _ <- symbol "="
    expr <- parseExpr
    pure (name, expr)

parseFunctionDeclarationWithImplementation :: Parser Expr
parseFunctionDeclarationWithImplementation = do
    (declName, typeExpr) <- parseFunctionDeclarationWithBody
    statementSeparator
    (implName, body) <- parseFunctionDeclarationWithBody

    if implName == declName
        then return $ Let declName typeExpr body (Var declName)
        else fail $ "Function name mismatch: expected " ++ declName ++ " but got " ++ implName


parseMatchStatement :: Parser Expr
parseMatchStatement = do
    _ <- reservedWord "match"
    a <- parseAtom
    _ <- reservedWord "with"
    matchBranches <- manyTill parseMatchStmtBranches (lookAhead (void (symbol ".") <|> void parseStatementStart))
    return $ Match a matchBranches

parseWildcard :: Parser Expr
parseWildcard = do
   _ <- symbol "_"
   return $ Var "_"
    
parseMatchStmtBranches :: Parser (Expr, Expr)
parseMatchStmtBranches = do
    _ <- symbol "|"
    a <- try parseWildcard <|> parseAtom
    _ <- symbol "->"
    t <- parseAtom
    return (a, t)
