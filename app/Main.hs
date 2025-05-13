module Main where
import           Control.Monad         (unless)
import           Control.Monad.State
import           Data.Functor.Identity
import           Data.Maybe            (fromMaybe)
import           Data.Void
import           Debug.Trace           (trace)
import           Expr                  (Expr (..))
import           Infer
import           Parser
import           Shared                (Context, Repl (..), ReplState,
                                        SubtypeContext, Variable)
import           System.IO
import           Text.Megaparsec       (ParseErrorBundle (ParseErrorBundle),
                                        Parsec, errorBundlePretty, parse)
-- emptyCtx :: Context
-- emptyCtx = [("A", (Universe 0, Nothing)), ("x", (Var "A", Nothing)), ("B", (Universe 1, Nothing)),
--             ("y", (Var "C", Nothing)),
--             ("C", (Universe 0, Nothing)),
--             ("z", (Var "B", Nothing))
--            ]

-- emptySubCtx:: SubtypeContext
-- emptySubCtx = [(Var "C", [Var "A"])]

data Command = TypeCheckFile | Ctx | Continue | Evaluate

getCommand :: String -> Command
getCommand ":f" = TypeCheckFile
getCommand ":c" = Ctx
getCommand _    = Continue

----------------------------------------------------------------------------------------------------------
-- TODO: REPL(?): evaluator.
-- Inductive types, match statements, prelude types (Int, Bool) + support for operations
-- Improve printing results in console. Conditionally allow debugging with a flag?
----------------------------------------------------------------------------------------------------------
-- FIXME: arrow types with lists are broken.
-- e.g `let flist : Int -> Int list = \xs : Int . 32;` wont work
----------------------------------------------------------------------------------------------------------


main :: IO ()
main = evalStateT repl (Repl
    [ ("A", (Universe 0, Nothing))    -- A : Type
    , ("x", (Var "A", Nothing))       -- x has type A, or x ∈ A
    , ("list", (Pi "A" (Universe 0) (Universe 0), Nothing)) -- list : ∀A: Type -> Type
    ] [] 0)

repl :: ReplState ()
repl                                         = do
    input <- liftIO read'
    unless (input                            == ":q") $ do
        case getCommand input of
            TypeCheckFile -> do
                result <- liftIO $ readFile "ex.ty"
                let lines' = lines result
                evalExpr (parse parseProgram "" result) -- FIXME read lines passed in prompt.
            Ctx -> do
                Repl { context               = ctx, subtype = subctx, fresh = fresh} <- get
                liftIO $ print (formatContext ctx)
            Continue -> evalExpr (parse parseProgram "" input) -- FIXME placeholder operation
        repl
    where
        evalExpr :: Either (ParseErrorBundle String Void) [Expr] -> ReplState ()
        evalExpr (Left err) = liftIO $ putStrLn $ errorBundlePretty err
        evalExpr (Right exprs) = do
            liftIO $ putStrLn $ "(DEBUG) original expressions to eat: " ++ show exprs
            replState <- get
            liftIO $ putStrLn $ "(DEBUG) replState context: " ++ show (formatContext $ context replState)

            forM_ (init exprs) $ \expr -> do
                currentState <- get
                liftIO $ putStrLn $ "(DEBUG) current processing expr: " ++ show expr
                let (inferredType, newState) = runState (infer expr) currentState
                put newState
                liftIO $ putStrLn $ "(DEBUG) Current inferred type: " ++ show inferredType

            unless (null exprs) $ do --NOTE: evaluate the last expression as the value of the whole program
                currentState <- get
                let lastExpr = last exprs
                liftIO $ putStrLn $ "(DEBUG) Processing last expr: " ++ show lastExpr
                let (inferredType, newState) = runState (infer lastExpr) currentState
                put newState

                Repl { context = ctx } <- get
                liftIO $ putStrLn $ "(DEBUG) Final context: " ++ show (formatContext ctx)

                liftIO $ putStrLn $ "(DEBUG) expr before normalization: " ++ show lastExpr
                let normalizedExpr = normalize ctx lastExpr
                liftIO $ putStrLn $ "(DEBUG) expr after normalization: " ++ show normalizedExpr
                liftIO $ print $ show normalizedExpr
                liftIO $ putStrLn $ "Type: " ++ show inferredType

read' :: IO String
read'                                        = putStr ">> " >> hFlush stdout >> getLine

formatContext :: Context -> [String]
formatContext                                = map fmt
    where
        fmt :: (Variable, (Expr, Maybe Expr)) -> String
        fmt (var, (typ, Nothing))            = show var ++ " : " ++ show typ
        fmt (var, (typ, Just val))           = show var ++ " : " ++ show typ ++ " = " ++ show val
