{-# LANGUAGE LambdaCase #-}
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
infer (BaseType (Integer n))                     = return $ Var "Int"
infer (BaseType (Boolean b))                     =  return $ BaseType (Boolean b)
infer (Universe k)                               = return $ Universe ( k + 1 )
infer (BinOp (BaseType (Integer a)) (BaseType (Integer b)))= infer (BaseType (Integer (a + b)))
infer (BinOp e1 e2)= do
    t1 <- infer e1
    t2 <- infer e2
    case (t1, t2) of
        (Var "Int", Var "Int") -> return $ Var "Int"
        _ -> error $ "Type mismatch in binary operation: " ++ show t1  ++ " and " ++ show t2
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
        newCtx                                   = extend x t Nothing ctx
    put replState { context                      = newCtx }
    te <- infer e
    return $ Pi x t te
infer (App e1 e2)                                = do
    trace ("Application case: " ++ show e1 ++ " applied to " ++ show e2) $ return ()
    replState <- get
    Repl { context                               = ctx, subtype = subctx, fresh = fresh} <- get
    let (stateResult, _)                         = runState (inferPi ctx subctx e1) replState
        (x, t1, t2)                              = stateResult
    t1' <- infer e2
    if isSubtype ctx subctx t1 t1'
        then do
            let (result, fresh')                 = runState (subst [(x, e2)] t2) fresh
            put replState { fresh                = fresh' }
            trace ("res " ++ show result) $ return ()
            return result
        else error $ "Couldn't match expected types: " ++ show t1 ++ " and " ++ show t1'
infer (Inductive name ty constructors) = do
    replState <- get
    tyType <- infer ty -- check that ty is well-typed in universe Ui
    ensureUniverse tyType

    let ctx = context replState
    let newCtx = extend name ty Nothing ctx -- Extend the context with nat : ty
    put replState { context = newCtx }

    forM_ constructors $ \(cname, ctype) -> do -- for each constructor ci, check if ti : nat
        ctypeTy <- infer ctype
        -- ensureUniverse ctypeTy -- Each constructor must be well-typed

        -- Check return type of constructor ends in the type being defined
        case getReturnType ctype of
            Just (Var retName) | retName == name -> do
                                     modify (\st -> st { context = extend cname ctype Nothing (context st) })
            _ -> error $ "Constructor " ++ cname ++ " must return " ++ name

    return $ Inductive name ty constructors
infer (Let x t e1 e2) = do
    replState <- get
    let ctx = context replState
    let subctx = subtype replState

    trace ("Let expression: " ++ show x ++ " : " ++ show t ++ " = " ++ show e1) $ return ()

    t1 <- infer e1
    trace ("Declared type: " ++ show t) $ return ()
    trace ("Inferred type of expression: " ++ show t1) $ return ()

    if isSubtype ctx subctx t1 t
       then do
         trace "Types match, extending context" $ return ()
         let newCtx = extend x t (Just e1) ctx
         put replState { context = newCtx }
         infer e2
       else error $ "Type mismatch in let expression: " ++ show t ++ " and " ++ show t1 ++ " are not equal."
infer (List _ []) = return $ error "Empty lists require type annotations"
infer (List _ (first:rest)) = do
    elementType <- infer first

    forM_ rest $ \element -> do
        elemType <- infer element
        replState <- get
        let ctx = context replState
        let subctx = subtype replState

        unless (isSubtype ctx subctx elemType elementType) $
            error $ "List elements must have the same type. Expected " ++
                   show elementType ++ " but got " ++ show elemType

    return $ App (Var "list") elementType
infer (Map f xs) = do
    replState <- get
    let ctx = context replState
    let subctx = subtype replState

    f' <- infer f
    xs' <- infer xs

    case xs' of
        App (Var "list") elemType -> do -- check if xs is an interable list of A
            case f' of
                Pi _ inType outType -> do -- if so, check that f' has type ∏_: A -> B
                    if isSubtype ctx subctx elemType inType -- check if list type is a substype of f input type..
                       then do
                        return $ App (Var "list") outType -- If correct, return [B]
                    else error $ "Type mismatch in map: function expects " ++
                                    show inType ++ " but list elements have type " ++
                                    show elemType
                _ -> error $ "Map requires a function, but got: " ++ show f'
        _ -> error $ "Map can only be applied to lists, but got: " ++ show xs'
infer x                                 =
       trace ("(INFERENCE) unhandled expression: " ++ show x) $ error "Caught unsupported expression."


ensureUniverse :: Expr -> State Repl()
ensureUniverse (Universe _) = pure ()
ensureUniverse x = error $ "Expected a universe type, got: " ++ show x

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

getReturnType :: Expr -> Maybe Expr
getReturnType (Pi _ _ body) = getReturnType body
getReturnType t             = Just t

normalize :: Context -> Expr -> Expr
normalize ctx (Var x)                            =
    trace ("(DEBUG) looking for " ++ show x) $
    case lookupVal x ctx of
        Just (Just t) -> normalize ctx t -- NOTE: if there is a value, keep normalizing
        _             -> Var x -- NOTE: otherwise return the variable as it is
normalize ctx (Pi x t e)                         =
    let (x', t', e')                             = normalizeAbstraction ctx (x, t, e)
    in Pi x' t' e'
normalize ctx (Lambda x t e)                     =
    let (x', t', e')                             = normalizeAbstraction ctx (x, t, e)
    in Lambda x' t' e'
normalize ctx (Universe k)                       = Universe k
normalize ctx (App e1 e2) =
    trace ("(DEBUG) App case: applying " ++ show e1 ++ " to " ++ show e2) $
    let e1' = normalize ctx e1
        e2' = normalize ctx e2
    in case e1' of
        Lambda x _ body ->
            trace ("(DEBUG) LAMBDA substituing " ++ show x ++ " for " ++ show e2' ++ " in " ++ show body) $
            normalize ctx (evalState (subst [(x, e2')] body) 0)
        _ ->
            case e1' of
                Var f -> case lookupVal f ctx of
                    Just (Just (Lambda x _ body)) ->
                        trace ("(DEBUG) case e1 Var " ++ show x ++ " with body " ++ show body) $
                        trace ("(DEBUG) substituing " ++ show x ++ " for " ++ show e2' ++ "in " ++ show body) $
                        normalize ctx (evalState (subst [(x, e2')] body) 0)
                    _ -> App e1' e2'
                _ -> App e1' e2'
normalize ctx (BaseType a) = BaseType a
normalize ctx (BinOp (BaseType (Integer a)) (BaseType (Integer b)))= BaseType (Integer (a + b))
normalize ctx (BinOp a b) = do
    let a' = normalize ctx a
        b' = normalize ctx b
    case (a', b') of
        (BaseType (Integer aVal), BaseType (Integer bVal)) -> BaseType (Integer (aVal + bVal))
        _ -> BinOp a' b'
normalize ctx (Let x t e1 e2)                  =
    let e1' = normalize ctx e1
        ctx' = extend x t (Just e1') ctx
        e2' = normalize ctx' e2
    in e2'
normalize ctx (Inductive name ty constructors) =
    let ty' = normalize ctx ty
        constructors' = [ (cname, normalize ctx ctype) | (cname, ctype) <- constructors ]
    in Inductive name ty' constructors'
normalize ctx (List ty elements) =
    let ty' = normalize ctx ty
        elements' = map (normalize ctx) elements
    in List ty' elements
normalize ctx (Map f xs) =
    let f' = normalize ctx f
        xs' = normalize ctx xs
    in case xs' of
        List elemType elems ->
            let results = map (\e -> normalize ctx (App f' e)) elems
            in List elemType results
        _ -> Map f' xs'
normalize ctx x                                 =
       trace ("(NORMALIZATION) unhandled expression: " ++ show x) $ error "Caught unsupported expression."

normalizeAbstraction :: Context -> (String, Expr, Expr) -> (String, Expr, Expr)
normalizeAbstraction ctx (x, t, e)               =
    let t'                                       = normalize ctx t
        ctx'                                     = extend x t' Nothing ctx
        e'                                       = normalize ctx' e
    in (x, t', e')


check :: Expr -> Expr -> State Repl ()
check (Lambda x t e) (Pi x' t1 t2) = do
    replState <- get
    let ctx                                      = context replState
    if equal ctx t t1
       then do
        let ctx' = extend x' t1 Nothing ctx
        put replState { context = ctx' }
        check e t2
    else error "Lambda argument type doesn't match"
check expr ty = do
    inferred <- infer expr
    ctx <- gets context
    subctx <- gets subtype
    unless (isSubtype ctx subctx inferred ty) $
        error $ "Type mismatch: expected " ++ show ty ++ ", but got " ++ show inferred
