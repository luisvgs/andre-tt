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
import           Text.Megaparsec.Char           (alphaNumChar, letterChar,
                                                 newline, space1, string)
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Char.Lexer     (space)
import           Text.Megaparsec.Debug

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

reservedWords :: [String]
reservedWords = ["Define", "Let", "function", "Type"]

reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

parseSubtype :: Parser Expr
parseSubtype = do
    _ <- reservedWord "Define"
    t1 <- parseExpr
    _ <- symbol "<:"
    t2 <- parseVariable
    return $ Subtype t1 t2

parseDefinition :: Parser Expr
parseDefinition = do
    _ <- reservedWord "Define"
    id <- identifier
    _ <- symbol ":"
    ty <- parseUniverse <|> parseVariable
    return $ Definition id ty

parseLet :: Parser Expr
parseLet = trace "parsing let exprs " $ do
    dbg "Consuming let keyword" $ reservedWord "Let"
    x <- dbg "consuming bing " $ pBind
    _ <- symbol ":"
    a <- parseExpr
    _ <- symbol "="
    t <- try parseLambda <|> parseExpr
    symbol ";"
    u <- parseExpr
    pure $ Let x a t u


pBind :: Parser String
pBind = trace "Parsing identifier or symbol" $ identifier <|> symbol "_"

parseVariable :: Parser Expr
parseVariable = Var <$> identifier

parseSpine :: Parser Expr
parseSpine = trace "Parsing spine" $ foldl1 App <$> some parseVariable

parseLambda :: Parser Expr
parseLambda = trace "Parsing lambda " $ do
    symbol "\\"
    var <- identifier
    _ <- symbol ":"
    varType <- parseExpr
    _ <- symbol "."
    body <- parseExpr
    return $ Lambda var varType body

parseUniverse :: Parser Expr
parseUniverse = do
    _ <- reservedWord "Type"
    Universe <$> L.decimal

parseStatement :: Parser Expr
parseStatement = trace "Trying to parse statement" $ try parseExpr <|> try parseDefinition <|> try parseSubtype

statementSeparator :: Parser ()
statementSeparator = void (symbol ";") <|> void newline

parseProgram :: Parser [Expr]
parseProgram = spaceConsumer *> sepEndBy parseStatement statementSeparator <* eof

parseExpr :: Parser Expr
parseExpr = trace "Parsing expr" $ parseLet <|> parseLambda <|> parseSpine <|> parseDefinition <|> parseSubtype <|> integer <|> boolean

integer :: Parser Expr
integer = do
    n <- lexeme L.decimal
    return (BaseType (Integer n))

boolean :: Parser Expr
boolean = do
    value <- choice [reservedWord "True" *> pure True, reservedWord "False" *> pure False]
    return (BaseType (Boolean value))
