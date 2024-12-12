module Main where
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)
import           Expr                (Expr (..))
import           Infer
import           Parser
import           Text.Megaparsec     (Parsec, errorBundlePretty, parse)


------------------
-- TODO: REPL(?), evaluator.
-- TODO: Inductive types, subtypes, match statements, prelude types (Int, Bool) + support for operations
------------------

main :: IO ()
main = do
    input <- readFile "ex.ty"
    case parse parseProgram "" input of
        Left err   -> putStrLn $ errorBundlePretty err
        Right expr -> print expr

test1 :: IO ()
test1 = do
    let ctx = [("x", (Universe 0, Nothing))]
    print $ infer ctx [] (Var "x")

test2 :: IO ()
test2 = do
    let ctx = []
    print $ infer ctx [] (Universe 0)

test3 :: IO ()
test3 = do
    let ctx = []
        t   = Universe 0
        e   = Var "x"
        lam = Lambda "x" t e
    print $ infer ctx [] lam

testLet :: Expr
testLet = Let "f" (Pi "a" (Var "A") (Var "A")) (Lambda "a" (Var "A") (Var "a")) (App (Var "f") (Var "x"))

testIdentity :: IO ()
testIdentity = do
    printInferredType emptyCtx testLet

printInferredType :: Context -> Expr -> IO ()
printInferredType ctx expr = do
    let inferredType = infer ctx [] expr
    putStrLn $ "type: " ++ show inferredType

testLetWithSubtype :: IO ()
testLetWithSubtype = do
    let expr = Let "f" (Pi "a" (Var "A") (Var "A")) (Lambda "a" (Var "A") (Var "a")) (App (Var "f") (Var "z"))
    let result = infer emptyCtx emptySubCtx expr
    putStrLn $ "Inferred type of testLetWithSubtype: " ++ show result
