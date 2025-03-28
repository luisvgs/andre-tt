module Parser where

import           BaseType
import           Control.Monad.Combinators.Expr (makeExprParser)
import           Data.Functor                   (void, ($>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Void
import           Expr                           (Expr (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char           (alphaNumChar, letterChar,
                                                 newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Char.Lexer     (space)
import           Text.Megaparsec.Debug
-- import           Debug.Trace

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

reservedWords :: [String]
reservedWords = ["define", "let", "function", "Type"]

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
parseLet = do
    reservedWord "let"
    x <- identifier
    _ <- symbol ":"
    a <- try parseArrowType <|> parseAtom
    _ <- symbol "="
    t <- parseExpr
    symbol ";"
    u <- parseExpr
    pure $ Let x a t u

parseLetDefinition :: Parser Expr
parseLetDefinition = do
    reservedWord "let"
    x <- identifier
    _ <- symbol ":"
    a <- try parseArrowType <|> parseAtom
    _ <- symbol "="
    t <- parseExpr
    _ <- symbol ";"
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

parseArrowType :: Parser Expr
parseArrowType = do
    t1 <- parseAtom
    _ <- symbol "->"
    t2 <- parseAtom
    let freshVar = "_x"
    return $ Pi freshVar t1 t2

parseStatement :: Parser Expr
parseStatement = try parseExpr <|> try parseDefinition <|> try parseSubtype <|> try parseLetDefinition

statementSeparator :: Parser ()
statementSeparator = void (symbol ";") <|> void newline

parseProgram :: Parser [Expr]
parseProgram = spaceConsumer *> sepEndBy parseStatement statementSeparator <* eof

parseExpr :: Parser Expr
parseExpr = try parseLet <|> try parseLambda <|> try parseSpine <|> try parseAtom

parseAtom :: Parser Expr
parseAtom = choice
    [ parseVariable
    , parseUniverse
    , integer
    , boolean
    , between (symbol "(") (symbol ")") parseExpr
    ]

integer :: Parser Expr
integer = do
    n <- lexeme L.decimal
    return (BaseType (Integer n))

boolean :: Parser Expr
boolean = do
    value <- choice [reservedWord "True" *> pure True, reservedWord "False" *> pure False]
    return (BaseType (Boolean value))
