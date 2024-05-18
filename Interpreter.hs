module Interpreter where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map

import Data.Function (fix)

import GeneratedParser.AbsEspresso

type Var = String
type Loc = Int
type ErrMsg = String
type Fun = [Value] -> IM Value

data Value = VInt Integer | VBool Bool | VStr String | VVoid | VTuple [Value] | VFun Fun

data IState = IState {
    env :: Map.Map Var Loc,
    store :: Map.Map Loc Value,
    latestLoc :: Loc
}

data Exit = EReturn Value | EBreak Pos | EContinue Pos

type IM = StateT IState (ExceptT ErrMsg IO)

type Pos = BNFC'Position

errMessage :: Pos -> String
errMessage (Just (l, c)) = "Error in line " ++ show l ++ ", column " ++ show c ++ ": "
errMessage Nothing = ""

throwErr :: Pos -> String -> IM a
throwErr pos msg = lift $ throwError $ errMessage pos ++ msg

convertIdent :: Ident -> Var
convertIdent (Ident x) = x

newLoc :: IM Loc
newLoc = do
    loc <- gets latestLoc
    modify $ \s -> s { latestLoc = loc + 1 }
    return $ loc + 1

getValue :: Pos -> Var -> IM Value
getValue pos x = do
    env <- gets env
    case Map.lookup x env of
        Just loc -> do
            store <- gets store
            case Map.lookup loc store of
                Just v -> return v
                Nothing -> throwErr pos $ "Internal interpreter error: nonexistent location"
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
addTopDef (FnDef pos t (Ident f) args b) = setNewValue f $ VFun $ fix $ \phi argVals -> do
    oldEnv <- gets env
    let argPairs = zip (map (\(Arg _ t (Ident x)) -> x) args) argVals
    mapM_ (\(x, v) -> setNewValue x v) argPairs
    setNewValue f $ VFun phi
    ret <- execBlock b
    modify $ \s -> s { env = oldEnv }
    case ret of
        Just (EReturn v) -> return v
        Just (EBreak pos) -> throwErr pos "Break outside loop"
        Just (EContinue pos) -> throwErr pos "Continue outside loop"
        _ -> case t of
            Void _ -> return VVoid
            _ -> throwErr pos "Function did not return a value"

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

execBlock :: Block -> IM (Maybe Exit)
execBlock (Block _ stmts) = do
    oldEnv <- gets env
    ret <- execStmts stmts
    modify $ \s -> s { env = oldEnv }
    return ret

execStmts :: [Stmt] -> IM (Maybe Exit)
execStmts [] = return Nothing
execStmts (stmt:stmts) = do
    ret <- execStmt stmt
    case ret of
        Just exit -> return $ Just exit
        Nothing -> execStmts stmts

evalIndHelper :: IndHelper -> IM Value
evalIndHelper (IndBase pos x i) = do
    t <- getValue pos $ convertIdent x
    case t of
        VTuple ts -> if i >= 0 && (fromInteger i) < length ts
            then return $ ts !! (fromInteger i)
            else throwErr pos "Internal error: Type error not caught by type-checker (index out of bounds)"
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected tuple)"
evalIndHelper (IndRec pos ih i) = do
    v <- evalIndHelper ih
    case v of
        VTuple ts -> if i >= 0 && (fromInteger i) < length ts
            then return $ ts !! (fromInteger i)
            else throwErr pos "Internal error: Type error not caught by type-checker (index out of bounds)"
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected tuple)"

execStmt :: Stmt -> IM (Maybe Exit)

execStmt (Empty _) = return Nothing

execStmt (BStmt _ b) = execBlock b

execStmt (Decl _ t items) = do
    mapM_ (execItem t) items
    return Nothing

execStmt (FDecl _ td) = do
    _ <- addTopDef td
    return Nothing

execStmt (Ass pos x e) = do
    v <- evalExpr e
    setValue pos (convertIdent x) v
    return Nothing

execStmt (Incr pos x) = do
    val <- getValue pos $ convertIdent x
    case val of
        VInt n -> do
            setValue pos (convertIdent x) (VInt (n + 1))
            return Nothing
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected int)"

execStmt (Decr pos x) = do
    val <- getValue pos $ convertIdent x
    case val of
        VInt n -> do
            setValue pos (convertIdent x) (VInt (n - 1))
            return Nothing
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected int)"

execStmt (Ret _ e) = do
    v <- evalExpr e
    return $ Just $ EReturn v

execStmt (VRet _) = do
    return $ Just $ EReturn VVoid

execStmt (Cond pos e s) = do
    v <- evalExpr e
    case v of
        VBool True -> execStmt s
        VBool False -> return Nothing
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected bool)"

execStmt (CondElse pos e s1 s2) = do
    v <- evalExpr e
    case v of
        VBool True -> execStmt s1
        VBool False -> execStmt s2
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected bool)"

execStmt (While pos e s) = do
    v <- evalExpr e
    case v of
        VBool True -> do
            ret <- execStmt s
            case ret of
                Just (EBreak _) -> return Nothing
                _ -> execStmt (While pos e s)
        VBool False -> return Nothing
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected bool)"

execStmt (SExp _ e) = do
    _ <- evalExpr e
    return Nothing

execStmt (Break pos) = do
    return $ Just $ EBreak pos

execStmt (Continue pos) = do
    return $ Just $ EContinue pos

evalExpr :: Expr -> IM Value
evalExpr (EVar pos x) = getValue pos $ convertIdent x

evalExpr (ELitInt _ n) = return $ VInt n

evalExpr (ELitTrue _) = return $ VBool True

evalExpr (ELitFalse _) = return $ VBool False

evalExpr (EApp pos f args) = do
    val <- getValue pos $ convertIdent f
    case val of
        VFun phi -> do
            argVals <- mapM evalExpr args
            phi argVals
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected function)"

evalExpr (Ind pos ih) = evalIndHelper ih

evalExpr (EString _ s) = return $ VStr s

evalExpr (Neg pos e) = do
    v <- evalExpr e
    case v of
        VInt n -> return $ VInt (-n)
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected integer)"

evalExpr (Not pos e) = do
    v <- evalExpr e
    case v of
        VBool b -> return $ VBool (not b)
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected boolean)"

evalExpr (EMul pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt n1, VInt n2) -> case op of
            Times _ -> return $ VInt $ n1 * n2
            Div pos' -> do
                if n2 == 0
                    then throwErr pos' "Division by zero"
                else return $ VInt $ n1 `div` n2
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected integers)" 

evalExpr (EAdd pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt n1, VInt n2) -> case op of
            Plus _ -> return $ VInt $ n1 + n2
            Minus _ -> return $ VInt $ n1 - n2
        (VStr s1, VStr s2) -> case op of
            Plus _ -> return $ VStr $ s1 ++ s2
            Minus _ -> throwErr pos "Internal error: Type error not caught by type-checker (subtraction on strings)"
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (incorrect addition/subtraction types)"

evalExpr (ERel pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt n1, VInt n2) -> case op of
            LTH _ -> return $ VBool $ n1 < n2
            LE _ -> return $ VBool $ n1 <= n2
            GTH _ -> return $ VBool $ n1 > n2
            GE _ -> return $ VBool $ n1 >= n2
            EQU _ -> return $ VBool $ n1 == n2
            NE _ -> return $ VBool $ n1 /= n2
        (VStr s1, VStr s2) -> case op of
            EQU _ -> return $ VBool $ s1 == s2
            NE _ -> return $ VBool $ s1 /= s2
        (VBool b1, VBool b2) -> case op of
            EQU _ -> return $ VBool $ b1 == b2
            NE _ -> return $ VBool $ b1 /= b2
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (incorrect comparison types)"

evalExpr (EAnd pos e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected booleans)"

evalExpr (EOr pos e1 e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
        _ -> throwErr pos "Internal error: Type error not caught by type-checker (expected booleans)"

evalExpr (ETuple _ es) = do
    vs <- mapM evalExpr es
    return $ VTuple vs

printInt :: [Value] -> IM Value
printInt [VInt n] = do
    liftIO $ print n
    return $ VInt n
printInt _ = throwErr Nothing "Internal error: Type error not caught by type-checker (incorrect call to printInt)"

printString :: [Value] -> IM Value
printString [VStr s] = do
    liftIO $ putStrLn s
    return $ VStr s
printString _ = throwErr Nothing "Internal error: Type error not caught by type-checker (incorrect call to printString)"

error :: [Value] -> IM Value
error [] = throwErr Nothing "runtime error"
error _ = throwErr Nothing "Internal error: Type error not caught by type-checker (incorrect call to error)"

readInt :: [Value] -> IM Value
readInt [] = do
    n <- liftIO $ readLn
    return $ VInt n
readInt _ = throwErr Nothing "Internal error: Type error not caught by type-checker (incorrect call to readInt)"

readString :: [Value] -> IM Value
readString [] = do
    s <- liftIO $ getLine
    return $ VStr s
readString _ = throwErr Nothing "Internal error: Type error not caught by type-checker (incorrect call to readString)"

execProgram :: Program -> IM Integer
execProgram (Program _ topDefs) = do
    setNewValue "printInt" $ VFun printInt
    setNewValue "printString" $ VFun printString
    setNewValue "error" $ VFun Interpreter.error
    setNewValue "readInt" $ VFun readInt
    setNewValue "readString" $ VFun readString
    mapM_ addTopDef topDefs
    val <- getValue Nothing "main"
    case val of
        VFun phi -> do
            ret <- phi []
            case ret of
                VInt n -> return n
                _ -> throwErr Nothing "main: Expected integer"
        _ -> throwErr Nothing "main: Expected function"

interpret :: Program -> IO (Either ErrMsg Integer)
interpret p = do
    res <- runExceptT $ runStateT (execProgram p) $ IState Map.empty Map.empty 0
    case res of
        Left err -> return $ Left err
        Right (n, _) -> return $ Right n