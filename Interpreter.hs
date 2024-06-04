module Interpreter where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as Map

import Data.Function (fix)

import GeneratedParser.AbsEspresso

type Var = String
type Loc = Int
type ErrMsg = String
type Env = Map.Map Var Loc
type Fun = [FunArg] -> IM Value

data FunArg = Val Value | Ref Loc

data Value = VInt Int | VBool Bool | VStr String | VVoid | VTuple [Value] | VFun Env Fun

data IState = IState {
    env :: Map.Map Var Loc,
    store :: Map.Map Loc Value,
    latestLoc :: Loc
}

data Exit = EReturn Value | EBreak Pos | EContinue Pos

type IM = StateT IState (ExceptT ErrMsg IO)

type Pos = BNFC'Position

getEnv :: IM (Map.Map Var Loc)
getEnv = gets env

setEnv :: Map.Map Var Loc -> IM ()
setEnv env = modify $ \s -> s { env = env }

getLatestLoc :: IM (Loc)
getLatestLoc = gets latestLoc

setLatestLoc :: Loc -> IM ()
setLatestLoc latestLoc = modify $ \s -> s { latestLoc = latestLoc }

tcErrorMsg :: String -> String
tcErrorMsg s = "Internal runtime error: Type error not caught by type-checker (" ++ s ++ ")"

errMessage :: Pos -> String
errMessage (Just (l, c)) = "Runtime error in line " ++ show l ++ ", column " ++ show c ++ ": "
errMessage Nothing = ""

throwErr :: Pos -> String -> IM a
throwErr pos msg = lift $ throwError $ errMessage pos ++ msg

convertIdent :: Ident -> Var
convertIdent (Ident x) = x

newLoc :: IM Loc
newLoc = do
    loc <- getLatestLoc
    setLatestLoc $ loc + 1
    return $ loc + 1

getValue :: Pos -> Var -> IM Value
getValue pos x = do
    env <- getEnv
    case Map.lookup x env of
        Just loc -> do
            store <- gets store
            case Map.lookup loc store of
                Just v -> return v
                Nothing -> throwErr pos $ "Internal interpreter error: nonexistent location"
        Nothing -> throwErr pos $ tcErrorMsg $ "variable " ++ x ++ " not in scope"

setValue :: Pos -> Var -> Value -> IM ()
setValue pos x v = do
    env <- getEnv
    case Map.lookup x env of
        Just loc -> modify $ \s -> s { store = Map.insert loc v (store s) }
        Nothing -> throwErr pos $ tcErrorMsg $ "variable " ++ x ++ " not in scope"

setNewValue :: Var -> Value -> IM ()
setNewValue x v = do
    loc <- newLoc
    modify $ \s -> s { env = Map.insert x loc (env s), store = Map.insert loc v (store s) }

setFuncValue :: Var -> FunArg -> IM ()
setFuncValue x (Val v) = setNewValue x v
setFuncValue x (Ref loc) = modify $ \s -> s { env = Map.insert x loc (env s) }

addTopDef :: TopDef -> IM ()
addTopDef (FnDef pos t (Ident f) args b) = do
    env <- getEnv
    setNewValue f $ VFun (env) $ fix $ \phi argVals -> do
        oldEnv <- getEnv
        let argPairs = zip (map (\(Arg _ t (Ident x)) -> x) args) argVals
        mapM_ (\(x, v) -> setFuncValue x v) argPairs
        setNewValue f $ VFun env phi
        ret <- execBlock b
        setEnv oldEnv
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
    oldEnv <- getEnv
    ret <- execStmts stmts
    setEnv oldEnv
    return ret

execStmts :: [Stmt] -> IM (Maybe Exit)
execStmts [] = return Nothing
execStmts (stmt:stmts) = do
    ret <- execStmt stmt
    case ret of
        Just exit -> return $ Just exit
        Nothing -> execStmts stmts

execIncrOrDecr :: Pos -> Ident -> (Int -> Int -> Int) -> IM (Maybe Exit)
execIncrOrDecr pos x op = do
    v <- getValue pos $ convertIdent x
    case v of
        VInt n -> do
            setValue pos (convertIdent x) (VInt (n `op` 1))
            return Nothing
        _ -> throwErr pos $ tcErrorMsg "expected int"

setTuple :: Pos -> AssLHS -> Value -> IM ()

setTuple pos (AssLSBase _ x) v = setValue pos (convertIdent x) v

setTuple pos (AssLSRec _ lhss) (VTuple vs) = do
    mapM_ (uncurry $ setTuple pos) $ zip lhss vs

setTuple pos _ _ = throwErr pos $ tcErrorMsg "incorrect tuple assignment"

execStmt :: Stmt -> IM (Maybe Exit)

execStmt (Empty _) = return Nothing

execStmt (BStmt _ b) = execBlock b

execStmt (Decl _ t items) = do
    mapM_ (execItem t) items
    return Nothing

execStmt (FDecl _ td) = do
    addTopDef td
    return Nothing

execStmt (Ass pos lhs e) = do
    v <- evalExpr e
    setTuple pos lhs v
    return Nothing

execStmt (Incr pos x) = execIncrOrDecr pos x (+)

execStmt (Decr pos x) = execIncrOrDecr pos x (-)

execStmt (Ret _ e) = do
    v <- evalExpr e
    return $ Just $ EReturn v

execStmt (VRet _) = do
    return $ Just $ EReturn VVoid

execStmt (Cond pos e s) = do
    v <- evalExpr e
    case v of
        VBool True -> execBlock $ Block Nothing [s]
        VBool False -> return Nothing
        _ -> throwErr pos $ tcErrorMsg "expected bool"

execStmt (CondElse pos e s1 s2) = do
    v <- evalExpr e
    case v of
        VBool True -> execBlock $ Block Nothing [s1]
        VBool False -> execBlock $ Block Nothing [s2]
        _ -> throwErr pos $ tcErrorMsg "expected bool"

execStmt (While pos e s) = do
    v <- evalExpr e
    case v of
        VBool True -> do
            ret <- execBlock $ Block Nothing [s]
            case ret of
                Just (EBreak _) -> return Nothing
                _ -> execStmt (While pos e s)
        VBool False -> return Nothing
        _ -> throwErr pos $ tcErrorMsg "expected bool"

execStmt (SExp _ e) = do
    evalExpr e
    return Nothing

execStmt (Break pos) = do
    return $ Just $ EBreak pos

execStmt (Continue pos) = do
    return $ Just $ EContinue pos

evalTupIndex :: Pos -> Value -> Int -> IM Value
evalTupIndex pos v i =
    case v of
        VTuple ts -> return $ ts !! i
        _ -> throwErr pos $ tcErrorMsg "expected tuple"

evalIndHelper :: IndHelper -> IM Value
evalIndHelper (IndBase pos x i) = do
    v <- getValue pos $ convertIdent x
    evalTupIndex pos v $ fromInteger i
evalIndHelper (IndRec pos ih i) = do
    v <- evalIndHelper ih
    evalTupIndex pos v $ fromInteger i

evalBinLogical :: Pos -> Expr -> (Bool -> Bool -> Bool) -> Expr -> IM Value
evalBinLogical pos e1 op e2 = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VBool b1, VBool b2) -> return $ VBool $ b1 `op` b2
        _ -> throwErr pos $ tcErrorMsg "expected bools"

compareNonFunction :: Value -> Value -> IM Bool
compareNonFunction (VInt n1) (VInt n2) = return $ n1 == n2
compareNonFunction (VBool b1) (VBool b2) = return $ b1 == b2
compareNonFunction (VStr s1) (VStr s2) = return $ s1 == s2
compareNonFunction (VTuple ts1) (VTuple ts2) = do
    bs <- mapM (uncurry compareNonFunction) $ zip ts1 ts2
    return $ and bs

evalPassedArg :: PassedArg -> IM FunArg

evalPassedArg (ArgVal _ e) = do
    v <- evalExpr e
    return $ Val v

evalPassedArg (ArgRef pos x) = do
    env <- getEnv
    case Map.lookup (convertIdent x) env of
        Just loc -> return $ Ref loc
        Nothing -> throwErr pos $ tcErrorMsg "variable not in scope"

evalExpr :: Expr -> IM Value
evalExpr (EVar pos x) = getValue pos $ convertIdent x

evalExpr (ELitInt _ n) = return $ VInt $ fromInteger n

evalExpr (ELitTrue _) = return $ VBool True

evalExpr (ELitFalse _) = return $ VBool False

evalExpr (EApp pos f args) = do
    val <- getValue pos $ convertIdent f
    case val of
        VFun env phi -> do
            argVals <- mapM evalPassedArg args
            oldEnv <- getEnv
            setEnv env
            ret <- phi argVals
            setEnv oldEnv
            return ret
        _ -> throwErr pos $ tcErrorMsg "expected function"

evalExpr (Ind pos ih) = evalIndHelper ih

evalExpr (EString _ s) = return $ VStr s

evalExpr (Neg pos e) = do
    v <- evalExpr e
    case v of
        VInt n -> return $ VInt (-n)
        _ -> throwErr pos $ tcErrorMsg "expected int"

evalExpr (Not pos e) = do
    v <- evalExpr e
    case v of
        VBool b -> return $ VBool (not b)
        _ -> throwErr pos $ tcErrorMsg "expected bool"

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
        _ -> throwErr pos $ tcErrorMsg "expected ints"

evalExpr (EAdd pos e1 op e2) = do
    v1 <- evalExpr e1
    v2 <- evalExpr e2
    case (v1, v2) of
        (VInt n1, VInt n2) -> case op of
            Plus _ -> return $ VInt $ n1 + n2
            Minus _ -> return $ VInt $ n1 - n2
        (VStr s1, VStr s2) -> case op of
            Plus _ -> return $ VStr $ s1 ++ s2
            Minus _ -> throwErr pos $ tcErrorMsg "subtraction on strings"
        _ -> throwErr pos $ tcErrorMsg "incorrect addition/subtraction types"

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
        (VTuple ts1, VTuple ts2) -> case op of
            EQU _ -> do
                b <- compareNonFunction v1 v2
                return $ VBool b
            NE _ -> do
                b <- compareNonFunction v1 v2
                return $ VBool $ not b
        _ -> throwErr pos $ tcErrorMsg "incorrect comparison types"

evalExpr (EAnd pos e1 e2) = evalBinLogical pos e1 (&&) e2

evalExpr (EOr pos e1 e2) = evalBinLogical pos e1 (||) e2

evalExpr (ETuple _ es) = do
    vs <- mapM evalExpr es
    return $ VTuple vs

printInt :: [FunArg] -> IM Value
printInt [Val (VInt n)] = do
    liftIO $ print n
    return VVoid
printInt [Ref loc] = do
    store <- gets store
    case Map.lookup loc store of
        Just (VInt n) -> do
            liftIO $ print n
            return VVoid
        _ -> throwErr Nothing $ tcErrorMsg "incorrect call to printInt"
printInt _ = throwErr Nothing $ tcErrorMsg "incorrect call to printInt"

printString :: [FunArg] -> IM Value
printString [Val (VStr s)] = do
    liftIO $ putStrLn s
    return VVoid
printString [Ref loc] = do
    store <- gets store
    case Map.lookup loc store of
        Just (VStr s) -> do
            liftIO $ putStrLn s
            return VVoid
        _ -> throwErr Nothing $ tcErrorMsg "incorrect call to printString"
printString _ = throwErr Nothing $ tcErrorMsg "incorrect call to printString"

error :: [FunArg] -> IM Value
error [] = throwErr Nothing "Runtime error: error() function called"
error _ = throwErr Nothing $ tcErrorMsg "incorrect call to error"

readInt :: [FunArg] -> IM Value
readInt [] = do
    n <- liftIO $ readLn
    return $ VInt n
readInt _ = throwErr Nothing $ tcErrorMsg "incorrect call to readInt"

readString :: [FunArg] -> IM Value
readString [] = do
    s <- liftIO $ getLine
    return $ VStr s
readString _ = throwErr Nothing $ tcErrorMsg "incorrect call to readString"

execProgram :: Program -> IM Int
execProgram (Program _ topDefs) = do
    setNewValue "printInt" $ VFun Map.empty printInt
    setNewValue "printString" $ VFun Map.empty printString
    setNewValue "error" $ VFun Map.empty Interpreter.error
    setNewValue "readInt" $ VFun Map.empty readInt
    setNewValue "readString" $ VFun Map.empty readString
    mapM_ addTopDef topDefs
    vmain <- getValue Nothing "main"
    case vmain of
        VFun _ phi -> do
            ret <- phi []
            case ret of
                VInt n -> return n
                _ -> throwErr Nothing $ tcErrorMsg "main has incorrect return type"
        _ -> throwErr Nothing $ tcErrorMsg "main is not a function"

interpret :: Program -> IO (Either ErrMsg Int)
interpret p = do
    let initState = IState Map.empty Map.empty 0
    res <- runExceptT $ runStateT (execProgram p) $ initState
    case res of
        Left err -> return $ Left err
        Right (n, _) -> return $ Right n