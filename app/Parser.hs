module Parser where

import           Control.Monad.Combinators.Expr (makeExprParser)
import           Data.Functor                   (($>))
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Void
import           Expr                           (Expr (..))
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Char.Lexer     (space)



type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

reservedWords :: [String]
reservedWords = ["Define", "let", "function", "Type"]

reservedWord :: String -> Parser ()
reservedWord w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x



parseDefinition :: Parser Expr
parseDefinition = do
    _ <- reservedWord "Define"
    id <- identifier
    _ <- symbol ":"
    ty <- parseUniverse
    return $ Definition id ty

parseLet :: Parser Expr
parseLet = do
    l <- reservedWord "let"
    x <- pBind
    symbol ":"
    a <- parseExpr
    symbol "="
    t <- parseExpr
    symbol ";"
    u <- parseExpr
    pure $ Let x a t u

pBind :: Parser String
pBind = identifier <|> symbol "_"

parseVariable :: Parser Expr
parseVariable = Var <$> identifier

parseSpine :: Parser Expr
parseSpine = foldl1 App <$> some parseVariable

parseLambda :: Parser Expr
parseLambda = do
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
    n <- L.decimal
    return $ Universe n


parseExpr :: Parser Expr
parseExpr = parseLet <|> parseLambda <|> parseSpine <|> parseDefinition
