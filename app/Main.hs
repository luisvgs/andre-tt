module Main where
import           Control.Monad         (unless)
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Maybe            (fromMaybe)
import           Debug.Trace           (trace)
import           Expr                  (Expr (..))
import           Infer
import           Parser
import           Shared                (Context, Repl (..), ReplState,
                                        SubtypeContext, Variable)
import           System.IO
import           Text.Megaparsec       (Parsec, errorBundlePretty, parse)

-- emptyCtx :: Context
-- emptyCtx = [("A", (Universe 0, Nothing)), ("x", (Var "A", Nothing)), ("B", (Universe 1, Nothing)),
--             ("y", (Var "C", Nothing)),
--             ("C", (Universe 0, Nothing)),
--             ("z", (Var "B", Nothing))
--            ]

-- emptySubCtx:: SubtypeContext
-- emptySubCtx = [(Var "C", [Var "A"])]

data Command = TypeCheckFile | Ctx | Continue

getCommand :: String -> Command
getCommand ":f" = TypeCheckFile
getCommand ":e" = Ctx
getCommand _    = Continue

------------------
-- TODO: REPL(?): evaluator.
-- TODO: Inductive types, match statements, prelude types (Int, Bool) + support for operations
------------------

main :: IO ()
main                                         = evalStateT repl (Repl [] [] 0)

repl :: ReplState ()
repl                                         = do
    input <- liftIO read'
    unless (input                            == ":q") $ do
        case getCommand input of
            TypeCheckFile -> do
                result <- liftIO $ readFile "ex.ty"
                liftIO $ putStrLn result
            Ctx -> do
                Repl { context               = ctx, subtype = subctx, fresh = fresh} <- get
                liftIO $ print (formatContext ctx)
            Continue -> case parse parseProgram "" input of
                Left err   -> liftIO $ putStrLn $ errorBundlePretty err
                Right expr -> do
                    let inferredExprs        = traverse infer expr
                    replState <- get
                    let (result, finalState) = runState inferredExprs replState
                    put finalState
                    let results              = runIdentity $ evalStateT inferredExprs finalState
                    liftIO $ print result
        repl

read' :: IO String
read'                                        = putStr ">> " >> hFlush stdout >> getLine

formatContext :: Context -> [String]
formatContext                                = map fmt
    where
        fmt :: (Variable, (Expr, Maybe Expr)) -> String
        fmt (var, (typ, Nothing))            = show var ++ " : " ++ show typ
        fmt (var, (typ, Just val))           = show var ++ " : " ++ show typ ++ " = " ++ show val

-- test1 :: IO ()
-- test1                                     = do
--     let ctx                               = [("x", (Universe 0, Nothing))]
--     print $ infer ctx [] (Var "x")

-- test2 :: IO ()
-- test2                                     = do
--     let ctx                               = []
--     print $ infer ctx [] (Universe 0)

-- test3 :: IO ()
-- test3                                     = do
--     let ctx                               = []
--         t                                 = Universe 0
--         e                                 = Var "x"
--         lam                               = Lambda "x" t e
--     print $ infer ctx [] lam

-- testLet :: Expr
-- testLet                                   = Let "f" (Pi "a" (Var "A") (Var "A")) (Lambda "a" (Var "A") (Var "a")) (App (Var "f") (Var "x"))

-- testIdentity :: IO ()
-- testIdentity                              = do
--     printInferredType emptyCtx testLet

-- printInferredType :: Context -> Expr -> IO ()
-- printInferredType ctx expr                = do
--     let inferredType                      = infer ctx [] expr
--     putStrLn $ "type: " ++ show inferredType

-- testLetWithSubtype :: IO ()
-- testLetWithSubtype                        = do
--     let expr                              = Let "f" (Pi "a" (Var "A") (Var "A")) (Lambda "a" (Var "A") (Var "a")) (App (Var "f") (Var "z"))
--     let result                            = infer emptyCtx emptySubCtx expr
--     putStrLn $ "Inferred type of testLetWithSubtype: " ++ show result
