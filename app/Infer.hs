module Infer where
import           BaseType
import           Control.Applicative (Alternative (..))
import           Control.Monad.State
import           Data.Maybe          (fromMaybe)
import           Debug.Trace         (trace)
import           Expr                (Expr (..))
import           Shared
import           Subst

isSubtype :: Context -> SubtypeContext -> Expr -> Expr -> Bool
isSubtype ctx subCtx e1 e2
    | equal ctx e1 e2                            = True
    | otherwise                                  = case lookupSubtypes e1 subCtx of
        Just supers -> any (\super -> isSubtype ctx subCtx super e2) supers
        Nothing     -> False

lookupTy :: Variable -> Context -> Maybe Expr
lookupTy x ctx                                   = fmap fst (lookup x ctx)

lookupSubtypes :: Expr -> SubtypeContext -> Maybe [Expr]
lookupSubtypes                                   = lookup

lookupVal :: Variable -> Context -> Maybe (Maybe Expr)
lookupVal x ctx                                  = fmap snd (lookup x ctx)

extend :: Variable -> Expr -> Maybe Expr -> Context -> Context
extend x t v ctx                                 = (x, (t, v)) : ctx

extendSubtype :: Expr -> Expr -> SubtypeContext -> SubtypeContext
extendSubtype e1 e2 subctx                       = (e1, [e2]) : subctx

equal :: Context -> Expr -> Expr -> Bool
equal ctx e1 e2                                  =
    let equal' e1 e2                             =
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

infer :: Expr -> State Repl Expr
infer (Var x)                                    = do
    Repl {subtype                                = subtype, context = currentCtx, fresh = fresh }<- get
    let inferredType                             = lookupTy x currentCtx <|> fmap head (lookupSubtypes (Var x) subtype)
    case inferredType of
        Just ty -> return ty
        Nothing -> error ("Unknown identifier " ++ x)
infer (Subtype e1 e2)                            = do
    replState <- get
    let subctx                                   = subtype replState
    let newSubtypes                              = extendSubtype e1 e2 subctx
    put replState { subtype                      = newSubtypes }
    return $ Subtype e1 e2
infer (Definition id ty)                         = do
    replState <- get
    let ctx                                      = context replState
    let newCtx                                   = extend id ty Nothing ctx
    put replState { context                      = newCtx }
    return $ Definition id ty
infer (BaseType (Integer n))                     =  return $ BaseType (Integer n)
infer (BaseType (Boolean b))                     =  return $ BaseType (Boolean b)
infer (Universe k)                               = return $ Universe ( k + 1 )

infer (Pi x t1 t2)                               = do
    Repl { context                               = ctx, subtype = subctx } <- get
    replState <- get
    let k1                                       = runState (inferUniverse ctx subctx t1) replState
        k2                                       = runState(inferUniverse (extend x t1 Nothing ctx) subctx t2) replState
    return $ Universe (max ( fst k1 ) ( fst k2 ))
infer (Lambda x t e)                             = do
    replState <- get
    let ctx                                      = context replState
        subctx                                   = subtype replState
        _                                        = inferUniverse ctx subctx t
        newCtx                                   = (extend x t Nothing ctx)
    put replState { context                      = newCtx }
    let (te, _)                                  = runState (infer e) replState
    return $ Pi x t te
infer (App e1 e2)                                = do
    replState <- get
    Repl { context                               = ctx, subtype = subctx, fresh = fresh} <- get
    let (stateResult, _)                         = runState (inferPi ctx subctx e1) replState
        (x, t1, t2)                              = stateResult
    t1' <- infer e2
    if isSubtype ctx subctx t1 t1'
        then do
            let (result, fresh')                 = runState (subst [(x, e2)] t2) fresh
            put replState { fresh                = fresh' }
            return result
        else error $ "Couldn't match expected types: " ++ show t1 ++ " and " ++ show t1'
infer (Let x t e1 e2)                   = do
    replState <- get
    let Repl { context                  = ctx, subtype = subctx } = replState
    let (t1, _)                         = runState (infer e1) replState
    if equal ctx t t1
       then do
        let newCtx                  = extend x t (Just e1) ctx
        put replState { context = newCtx }
        infer e2
       else error "Type mismatch in let expression"
infer x                                 =
       trace ("Unhandled " ++ show x) $
           error "Unsupported expression "

inferUniverse :: Context -> SubtypeContext -> Expr -> State Repl Int
inferUniverse ctx subctx t                       = do
    replState <- get
    case normalize ctx (fst (runState (infer t) replState)) of
        Universe k -> return $ k
        _          -> error "Type expected"

inferPi :: Context -> SubtypeContext -> Expr -> State Repl (String, Expr, Expr)
inferPi ctx subctx e                             = do
    replState <- get
    case normalize ctx (fst (runState (infer e) replState)) of
        (Pi x t e) -> return $ (x, t, e)
        _          -> error "Function expected"

normalize :: Context -> Expr -> Expr
normalize ctx (Var x)                            =
    case lookupTy x ctx of
        Just t  -> normalize ctx t
        Nothing -> Var x
normalize ctx (Pi x t e)                         =
    let (x', t', e')                             = normalizeAbstraction ctx (x, t, e)
    in Pi x' t' e'
normalize ctx (Lambda x t e)                     =
    let (x', t', e')                             = normalizeAbstraction ctx (x, t, e)
    in Lambda x' t' e'
normalize ctx (Universe k)                       = Universe k
normalize ctx (App e1 e2)                        =
        let e2'                                  = normalize ctx e2
        in case normalize ctx e1 of
             Lambda x _ e1' -> normalize ctx (evalState (subst [(x, e2')] e1') 0)
             e1'            -> App e1' e2'

normalizeAbstraction :: Context -> (String, Expr, Expr) -> (String, Expr, Expr)
normalizeAbstraction ctx (x, t, e)               =
    let t'                                       = normalize ctx t
        ctx'                                     = extend x t' Nothing ctx
        e'                                       = normalize ctx' e
    in (x, t', e')
