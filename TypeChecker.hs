module TypeChecker where

import Control.Monad.State

import qualified Data.Map as Map

import GeneratedParser.AbsEspresso

data TType = TInt | TBool | TStr | TVoid | TTuple [TType] | TFun TType [TType] Block deriving Eq

data ItemType = TType | AnyType deriving Eq

type Var = String
type ErrMsg = String

data TCState = TCState { 
    env :: Map.Map Var TType,
    latestRet :: TType
}

type TCM = StateT TCState (Either ErrMsg)

type Pos = BNFC'Position

getEnv :: TCM (Map.Map Var TType)
getEnv = gets env

setEnv :: Map.Map Var TType -> TCM ()
setEnv env = modify $ \s -> s { env = env }

getLatestRet :: TCM TType
getLatestRet = gets latestRet

setLatestRet :: TType -> TCM ()
setLatestRet t = modify $ \s -> s { latestRet = t }

errMessage :: Pos -> String
errMessage (Just (l, c)) = "Error in line " ++ show l ++ ", column " ++ show c ++ ": "
errMessage Nothing = ""

throwErr :: Pos -> String -> TCM a
throwErr pos msg = lift $ Left $ errMessage pos ++ msg

convertIdent :: Ident -> Var
convertIdent (Ident x) = x

convertType :: Type -> TType
convertType (Int _) = TInt
convertType (Bool _) = TBool
convertType (Str _) = TStr
convertType (Void _) = TVoid
convertType (Tuple _ ts) = TTuple $ map convertType ts

getVarType :: Pos -> Var -> TCM TType
getVarType pos x = do
    env <- getEnv
    case Map.lookup x env of
        Just t -> return t
        Nothing -> throwErr pos $ "Variable " ++ x ++ " not in scope"

getIdentType :: Pos -> Ident -> TCM TType
getIdentType pos x = getVarType pos $ convertIdent x

setType :: Var -> TType -> TCM ()
setType x t = modify $ \s -> s { env = Map.insert x t (env s) }

checkBlock :: Block -> TCM ()
checkBlock (Block _ stmts) = do
    oldEnv <- getEnv
    mapM_ checkStmt stmts
    setEnv oldEnv

checkFunc :: TopDef -> TCM ()
checkFunc (FnDef _ t (Ident f) args b) = do
    oldEnv <- getEnv
    oldRet <- getLatestRet
    let argPairs = map (\(Arg _ t (Ident x)) -> (x, convertType t)) args
    mapM_ (\(x, t) -> setType x t) argPairs
    setLatestRet $ convertType t
    checkBlock b
    setEnv oldEnv
    setLatestRet oldRet

getItemIdent :: Item -> TCM (Pos, Var, TType, ItemType)
getItemIdent (NoInit pos (Ident x)) = return (pos, x, TInt, AnyType)
getItemIdent (Init pos (Ident x) e) = do
    t <- checkExpr e
    return (pos, x, t, TType)

checkItem :: TType -> Item -> TCM ()
checkItem t item = do
    (pos, x, t', chk) <- getItemIdent item
    if chk == AnyType
        then setType x t
    else if t == t'
        then setType x t
    else throwErr pos "Type mismatch in declaration"

addTopDef :: TopDef -> TCM ()
addTopDef (FnDef _ t (Ident f) args b) = do
    let argTypes = map (\(Arg _ t _) -> convertType t) args
    setType f $ TFun (convertType t) argTypes b

checkStmt :: Stmt -> TCM ()

checkStmt (Empty _) = return ()

checkStmt (BStmt pos b) = return checkBlock pos b

checkStmt (Decl pos t items) = do
    let t' = convertType t
    if t' == TVoid
        then throwErr pos "Cannot declare void"
    else do
        mapM_ (checkItem t') items
        return ()

checkStmt (FDecl _ td) = do
    addTopDef td
    checkFunc td
    return ()

checkStmt (Ass pos x e) = do
    t <- getIdentType pos x
    t' <- checkExpr e
    if t == t'
        then setType (convertIdent x) t
    else throwErr pos "Type mismatch in assignment"

checkStmt (Incr pos x) = do
    t <- getIdentType pos x
    if t /= TInt
        then throwErr pos "Attempted to increment a non-integer"
    else return ()

checkStmt (Decr pos x) = do
    t <- getIdentType pos x
    if t /= TInt
        then throwErr pos "Attempted to decrement a non-integer"
    else return ()

checkStmt (Ret pos e) = do
    t <- checkExpr e
    latestRet <- getLatestRet
    if t /= latestRet
        then throwErr pos "Incorrect return type"
    else return ()

checkStmt (VRet pos) = do
    latestRet <- getLatestRet
    if latestRet /= TVoid
        then throwErr pos "Incorrect return type"
    else return ()

checkStmt (Cond pos e s) = do
    t <- checkExpr e
    if t /= TBool
        then throwErr pos "Non-boolean condition"
    else do
        checkBlock (Block Nothing [s])


checkStmt (CondElse pos e s1 s2) = do
    t <- checkExpr e
    if t /= TBool
        then throwErr pos "Non-boolean condition"
    else do
        checkBlock (Block Nothing [s1])
        checkBlock (Block Nothing [s2])

checkStmt (While pos e s) = do
    t <- checkExpr e
    if t /= TBool
        then throwErr pos "Non-boolean condition"
    else do
        checkBlock (Block Nothing [s])

checkStmt (SExp _ e) = do
    checkExpr e
    return ()

checkStmt (Break _) = return ()

checkStmt (Continue _) = return ()

checkTupIndex :: Pos -> TType -> Integer -> TCM TType
checkTupIndex pos (TTuple ts) i = 
    if i >= 0 && (fromInteger i) < length ts
        then return $ ts !! (fromInteger i)
    else throwErr pos "Index out of bounds"
checkTupIndex pos _ _ = throwErr pos "Indexing on non-tuple"

checkIndHelper :: IndHelper -> TCM TType
checkIndHelper (IndBase pos x i) = do
    t <- getIdentType pos x
    checkTupIndex pos t i
checkIndHelper (IndRec pos ih i) = do
    t <- checkIndHelper ih
    checkTupIndex pos t i

checkBinLogical :: Pos -> Expr -> Expr -> TCM TType
checkBinLogical pos e1 e2 = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    case (t1, t2) of
        (TBool, TBool) -> return TBool
        _ -> throwErr pos "Logical operation on non-booleans"

checkExpr :: Expr -> TCM TType
checkExpr (EVar pos x) = getIdentType pos x

checkExpr (ELitInt _ _) = return TInt

checkExpr (ELitTrue _) = return TBool

checkExpr (ELitFalse _) = return TBool

checkExpr (EApp pos f args) = do
    fType <- getIdentType pos f
    case fType of
        TFun retType argTypes block -> do
            argTypes' <- mapM checkExpr args
            if argTypes == argTypes'
                then return retType
            else throwErr pos "Type mismatch in function application"
        _ -> throwErr pos "Function application on non-function"

checkExpr (Ind pos ih) = checkIndHelper ih

checkExpr (EString _ _) = return TStr

checkExpr (Neg pos e) = do
    t <- checkExpr e
    if t == TInt
        then return TInt
    else throwErr pos "Arithmetical negation on non-integer"

checkExpr (Not pos e) = do
    t <- checkExpr e
    if t == TBool
        then return TBool
    else throwErr pos "Logical negation on non-boolean"

checkExpr (EMul pos e1 _ e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        _ -> throwErr pos "Multiplication on non-integers"

checkExpr (EAdd pos e1 op e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    case (t1, t2) of
        (TInt, TInt) -> return TInt
        (TStr, TStr) -> do
            case op of
                Plus _ -> return TStr
                _ -> throwErr pos "Attempted to subtract two strings"
        _ -> throwErr pos "Incorrect addition/subtraction types"

checkExpr (ERel pos e1 op e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    case (t1, t2) of
        (TInt, TInt) -> return TBool
        (TBool, TBool) -> isEqOp op
        (TStr, TStr) -> isEqOp op
        _ -> throwErr pos "Comparison on illegal types"
    where 
        isEqOp :: RelOp -> TCM TType
        isEqOp op = case op of
            EQU _ -> return TBool
            NE _ -> return TBool
            _ -> throwErr pos "Incorrect comparison operator"

checkExpr (EAnd pos e1 e2) = checkBinLogical pos e1 e2

checkExpr (EOr pos e1 e2) = checkBinLogical pos e1 e2

checkExpr (ETuple _ es) = do
    ts <- mapM checkExpr es
    return $ TTuple ts

checkProgram :: Program -> TCM ()
checkProgram (Program pos topDefs) = do
    setType "printInt" $ TFun TVoid [TInt] $ Block pos []
    setType "printString" $ TFun TVoid [TStr] $ Block pos []
    setType "readInt" $ TFun TInt [] $ Block pos []
    setType "readString" $ TFun TStr [] $ Block pos []
    setType "error" $ TFun TVoid [] $ Block pos []
    mapM_ addTopDef topDefs
    env <- getEnv
    mainExists <- return $ Map.member "main" env
    if not mainExists
        then throwErr pos "No main function"
    else do
        f <- getIdentType pos $ Ident "main"
        case f of
            TFun TInt [] _ -> do
                mapM_ checkFunc topDefs
            _ -> throwErr pos "Main function has incorrect type"

typeCheck :: Program -> Either ErrMsg ()
typeCheck p = evalStateT (checkProgram p) $ initState where
    initState = TCState Map.empty TInt