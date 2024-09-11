module Main where
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           Expr                (Expr (..))
import           Parser
import           Text.Megaparsec     (Parsec, errorBundlePretty, parse)

type Variable = String

type Context = [(Variable, (Expr, Maybe Expr))]

lookupTy :: Variable -> Context -> Maybe Expr
lookupTy x ctx = fmap fst (lookup x ctx)

lookupVal :: Variable -> Context -> Maybe (Maybe Expr)
lookupVal x ctx = fmap snd (lookup x ctx)

extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend x t v ctx = (x, (t, v)) : ctx


refresh :: String -> State Int String
refresh x = do
    k <- get
    put (k + 1)
    return (x ++ show k)

type Substition = [(String, Expr)]

subst :: Substition -> Expr -> State Int Expr
subst s (Var x) = return $ fromMaybe (Var x) (lookup x s)
subst _ (Universe k) = return $ Universe k
subst s (Pi x a b) = substAbstraction s (x, a, b) >>= \(x', a', b') -> return (Pi x' a' b')
subst s (Lambda x a b) = substAbstraction s (x, a, b) >>= \(x', a', b') -> return (Lambda x' a' b')
subst s (App e1 e2) = do
    e1' <- subst s e1
    e2' <- subst s e2
    return (App e1' e2')

substAbstraction :: Substition -> (String, Expr, Expr) -> State Int (String, Expr, Expr)
substAbstraction s (x, t, e) = do
    x' <- refresh x
    t' <- subst s t
    e' <- subst ((x, Var x') : s) e
    return (x', t', e')

infer :: Context -> Expr -> Expr
infer ctx (Var x)      = fromMaybe (error "Unknown identifier") (lookupTy x ctx)
infer ctx (Universe k) = Universe ( k + 1 )
infer ctx (Pi x t1 t2) =
    let k1 = inferUniverse ctx t1
        k2 = inferUniverse (extend x t1 Nothing ctx) t2
    in Universe (max k1 k2)
infer ctx (Lambda x t e) =
    let t1 = inferUniverse ctx t
        te = infer (extend x t Nothing ctx) e
    in Pi x t te


inferUniverse :: Context -> Expr -> Int
inferUniverse ctx t =
    case normalize ctx ( infer ctx t ) of
        Universe k -> k
        _          -> error "Type expected"

inferPi :: Context -> Expr -> (String, Expr, Expr)
inferPi ctx e =
    case normalize ctx (infer ctx e) of
        (Pi x t e) -> (x, t, e)
        _          -> error "Function expected"

normalize :: Context -> Expr -> Expr
normalize ctx (Var x) = case lookupVal x ctx of
    Just (Just e) -> normalize ctx e
    Just Nothing  -> Var x
    Nothing       -> Var x
normalize ctx (Pi x t e) =
    let (x', t', e') = normalizeAbstraction ctx (x, t, e)
    in Pi x' t' e'
normalize ctx (Lambda x t e) =
    let (x', t', e') = normalizeAbstraction ctx (x, t, e)
    in Lambda x' t' e'
normalize ctx (Universe k) = Universe k
normalize ctx (App e1 e2) =
        let e2' = normalize ctx e2
        in case normalize ctx e1 of
             Lambda x _ e1' -> normalize ctx (evalState (subst [(x, e2')] e1') 0)
             e1'            -> App e1' e2'

normalizeAbstraction :: Context -> (String, Expr, Expr) -> (String, Expr, Expr)
normalizeAbstraction ctx (x, t, e) =
    let t' = normalize ctx t
        ctx' = extend x t' Nothing ctx
        e'  = normalize ctx' e
    in (x, t', e')

equal :: Context -> Expr -> Expr -> Bool
equal ctx e1 e2 =
    let equal' e1 e2 =
            case (e1, e2) of
                (Var x1, Var x2)           -> x1 == x2
                (App e11 e12, App e21 e22) -> equal' e11 e21 && equal' e12 e22
                (Universe k1, Universe k2) -> k1 == k2
                (Pi x1 t1 e1, Pi x2 t2 e2) -> equalAbstraction (x1, t1, e1) (x2, t2, e2)
                (Lambda x1 t1 e1, Lambda x2 t2 e2) -> equalAbstraction (x1, t1, e1) (x2, t2, e2)
                _                          -> False
        equalAbstraction (x, t1, e1) (y, t2, e2) =
            equal' t1 t2 && equal' e1 (evalState (subst [(y, Var x)] e2) 0)
    in equal' (normalize ctx e1) (normalize ctx e2)

-- Few tests

test1 :: IO ()
test1 = do
    let ctx = [("x", (Universe 0, Nothing))]
    print $ infer ctx (Var "x")

test2 :: IO ()
test2 = do
    let ctx = []
    print $ infer ctx (Universe 0) -- Should output Universe 1

test3 :: IO ()
test3 = do
    let ctx = []
        t   = Universe 0
        e   = Var "x"
        lam = Lambda "x" t e
    print $ infer ctx lam -- Should output Pi "x" (Universe 0) (Universe 0)


-- Define the identity function
identityType :: Expr
identityType = Pi "A" (Universe 0) (Pi "x" (Var "A") (Var "A"))

identityFunction :: Expr
identityFunction = Lambda "A" (Universe 0) (Lambda "x" (Var "A") (Var "x"))

testIdentity :: IO ()
testIdentity = do
    let ctx = []
    printInferredType ctx identityType

prettyPrint :: Expr -> String
prettyPrint (Var x) = x
prettyPrint (Universe k) = "Type" ++ show k
prettyPrint (Pi x t e) = "Pi (" ++ x ++ " : " ++ prettyPrint t ++ ") -> " ++ prettyPrint e
prettyPrint (Lambda x t e) = "Lambda (" ++ x ++ " : " ++ prettyPrint t ++ ") -> " ++ prettyPrint e
prettyPrint (App e1 e2) = "(" ++ prettyPrint e1 ++ " " ++ prettyPrint e2 ++ ")"
prettyPrint (Gensym s n) = s ++ show n
prettyPrint Dummy = "Dummy"

printInferredType :: Context -> Expr -> IO ()
printInferredType ctx expr = do
    let inferredType = infer ctx expr
    putStrLn $ "type: " ++ prettyPrint inferredType

-- TODO:
-- Write some tests
-- Write a proper REPL

main :: IO ()
main = do
    -- let input = "Define A : Type 0"
    let input = "\\a : A . a"
    case parse parseExpr "" input of
        Left err   -> putStrLn $ errorBundlePretty err
        Right expr -> print expr
