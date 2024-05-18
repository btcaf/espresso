module Interpreter where

import Control.Monad.State

import qualified Data.Map as Map

import GeneratedParser.AbsEspresso

type Var = String
type Loc = Int
type ErrMsg = String

data Value = VInt Integer | VBool Bool | VStr String | VTuple [Value] | VFun [Value] -> IM Value

data IState = IState {
    env :: Map.Map Var Loc,
    store :: Map.Map Loc Value,
    latestLoc :: Loc
}

data Exit = Ret Value | Break | Continue

type IM = StateT IState (Either ErrMsg)

type Pos = BNFC'Position

errMessage :: Pos -> String
errMessage (Just (l, c)) = "Error in line " ++ show l ++ ", column " ++ show c ++ ": "
errMessage Nothing = ""

throwErr :: Pos -> String -> TCM a
throwErr pos msg = lift $ Left $ errMessage pos ++ msg

convertIdent :: Ident -> Var
convertIdent (Ident x) = x

newLoc :: IM Loc
newLoc = do
    loc <- gets latestLoc
    modify $ \s -> s { latestLoc = loc + 1 }
    return loc + 1

getValue :: Pos -> Var -> IM Value
getValue pos x = do
    env <- gets env
    case Map.lookup x env of
        Just loc -> do
            store <- gets store
            case Map.lookup loc store of
                Just v -> return v
                Nothing -> throwErr pos $ "Internal interpreter error"
        Nothing -> throwErr pos $ "Variable " ++ x ++ " not in scope"

setValue :: Pos -> Var -> Value -> IM ()
setValue pos x v = do
    env <- gets env
    case Map.lookup x env of
        Just loc -> modify $ \s -> s { store = Map.insert loc v (store s) }
        Nothing -> throwErr pos $ "Variable " ++ x ++ " not in scope"

setNewValue :: Var -> Value -> IM ()
setNewValue x v = do
    loc <- newLoc
    modify $ \s -> s { env = Map.insert x loc (env s), store = Map.insert loc v (store s) }

addTopDef :: TopDef -> IM ()
addTopDef (FnDef _ t (Ident f) args b) = setNewValue f $ VFun $ fix $ \phi argVals -> do
    oldEnv <- gets env
    let argPairs = zip (map (\(Arg _ t (Ident x)) -> x) args) argVals
    mapM_ (\(x, v) -> setNewValue x v) argPairs
    setNewValue f $ VFun phi
    execBlock b
    modify $ \s -> s { env = oldEnv }

defaultValue :: Type -> Value
defaultValue (Int _) = VInt 0
defaultValue (Bool _) = VBool False
defaultValue (Str _) = VStr ""
defaultValue (Tuple _ ts) = VTuple $ map defaultValue ts

execItem :: Type -> Item -> IM ()
execItem t (NoInit _ x) = setNewValue (convertIdent x) $ defaultValue t
execItem t (Init _ x e) = do
    v <- evalExpr e
    setNewValue (convertIdent x) v

execBlock :: Block -> IM ()
execBlock (Block _ stmts) = do
    oldEnv <- gets env
    mapM_ execStmt
    execStmt stmts
    modify $ \s -> s { env = oldEnv }

execStmt :: Stmt -> IM ()

execStmt (Empty _) = return ()

execStmt (BStmt _ b) = execBlock b

execStmt (Decl _ t items) = do
    mapM_ (execItem t) items

execStmt (FDecl _ td) = addTopDef td

execStmt (Ass _ x e) = do
    v <- evalExpr e
    setValue pos x v

execStmt (Incr pos x) = do
    VInt n <- getValue pos x
    setValue pos x (VInt (n + 1))

execStmt (Decr pos x) = do
    VInt n <- getValue pos x
    setValue pos x (VInt (n - 1))

execStmt (Ret _ e) = do
    v <- evalExpr e
    lift $ Right v

execStmt (SExp _ e) = do
    evalExpr e
    return ()

evalExpr :: Expr -> IM Value
evalExpr (EVar pos x) = getValue pos x

evalExpr (ELitInt _ n) = return $ VInt n

evalExpr (ELitTrue _) = return $ VBool True

evalExpr (ELitFalse _) = return $ VBool False

evalExpr (EString _ s) = return $ VStr s

evalExpr (ETuple _ es) = do
    vs <- mapM evalExpr es
    return $ VTuple vs

evalExpr (EApp pos f args) = do
    VFun phi <- getValue pos f
    argVals <- mapM evalExpr args
    phi argVals

evalExpr (Neg pos e) = do
    v <- evalExpr e
    case v of
        VInt n -> return $ VInt (-n)
        _ -> throwErr pos "Expected integer"

evalExpr (Not pos e) = do
    v <- evalExpr e
    case v of
        VBool b -> return $ VBool (not b)
        _ -> throwErr pos "Expected boolean"

-- TODO EMul, etc