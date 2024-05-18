module TypeChecker where

import Control.Monad.State

import qualified Data.Map as Map

import GeneratedParser.AbsEspresso

data TType = TInt | TBool | TStr | TTuple [TType] | TFun TType [TType] Block deriving Eq

data ItemType = TType | AnyType deriving Eq

type Var = String
type ErrMsg = String

data TCState = TCState { 
    env :: Map.Map Var TType,
    latestRet :: TType
}

type TCM = StateT TCState (Either ErrMsg)

type Pos = BNFC'Position

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
convertType (Tuple _ ts) = TTuple $ map convertType ts

getType :: Pos -> Var -> TCM TType
getType pos x = do
    env <- gets env
    case Map.lookup x env of
        Just t -> return t
        Nothing -> throwErr pos $ "Variable " ++ x ++ " not in scope"

setType :: Var -> TType -> TCM ()
setType x t = modify $ \s -> s { env = Map.insert x t (env s) }

checkBlock :: Block -> TCM ()
checkBlock (Block _ stmts) = do
    oldEnv <- gets env
    mapM_ checkStmt stmts
    modify $ \s -> s { env = oldEnv }

checkFunc :: TopDef -> TCM ()
checkFunc (FnDef _ t (Ident f) args b) = do
    oldEnv <- gets env
    oldRet <- gets latestRet
    let argPairs = map (\(Arg _ t (Ident x)) -> (x, convertType t)) args
    mapM_ (\(x, t) -> setType x t) argPairs
    modify $ \s -> s { latestRet = convertType t }
    checkBlock b
    modify $ \s -> s { env = oldEnv, latestRet = oldRet }

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

checkStmt (Decl _ t items) = do
    let t' = convertType t
    mapM_ (checkItem t') items
    return ()

checkStmt (FDecl _ td) = do
    addTopDef td
    checkFunc td
    return ()

checkStmt (Ass pos x e) = do
    t <- getType pos $ convertIdent x
    t' <- checkExpr e
    if t == t'
        then setType (convertIdent x) t
    else
        throwErr pos "Type mismatch in assignment"

checkStmt (Incr pos x) = do
    t <- getType pos $ convertIdent x
    if t /= TInt
        then throwErr pos "Attempted to increment a non-integer"
    else
        return ()

checkStmt (Decr pos x) = do
    t <- getType pos $ convertIdent x
    if t /= TInt
        then throwErr pos "Attempted to decrement a non-integer"
    else
        return ()

checkStmt (Ret pos e) = do
    t <- checkExpr e
    latestRet <- gets latestRet
    if t /= latestRet
        then throwErr pos "Incorrect return type"
    else
        return ()

checkStmt (VRet _) = return ()
checkStmt (Cond _ _ _) = return ()
checkStmt (CondElse _ _ _ _) = return ()
checkStmt (While _ _ _) = return ()

-- TODO VRet, Cond, CondElse, While

checkStmt (SExp _ e) = do
    _ <- checkExpr e
    return ()

checkStmt (Break _) = return ()

checkStmt (Continue _) = return ()

checkExpr :: Expr -> TCM TType
checkExpr (EVar pos x) = getType pos $ convertIdent x

checkExpr (ELitInt _ _) = return TInt

checkExpr (ELitTrue _) = return TBool

checkExpr (ELitFalse _) = return TBool

checkExpr (EApp pos f args) = do
    fType <- getType pos $ convertIdent f
    case fType of
        TFun retType argTypes block -> do
            argTypes' <- mapM checkExpr args
            if argTypes == argTypes'
                then do
                    return retType
            else throwErr pos "Type mismatch in function application"
        _ -> throwErr pos "Function application on non-function"

-- TODO ind

checkExpr (EString _ _) = return TStr

checkExpr (Neg pos e) = do
    t <- checkExpr e
    if t == TInt
        then return TInt
    else throwErr pos "Negation on non-integer"

checkExpr (Not pos e) = do
    t <- checkExpr e
    if t == TBool
        then return TBool
    else throwErr pos "Negation on non-boolean"

checkExpr (EMul pos e1 _ e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if t1 == TInt && t2 == TInt
        then return TInt
    else throwErr pos "Multiplication on non-integers"

checkExpr (EAdd pos e1 _ e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if t1 == TInt && t2 == TInt
        then return TInt
    else if t1 == TStr && t2 == TStr
        then return TStr
    else throwErr pos "Incorrect addition types"

checkExpr (ERel pos e1 op e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if (t1 == TBool && t2 == TBool) || (t1 == TStr && t2 == TStr)
        then case op of
            EQU _ -> return TBool
            NE _ -> return TBool
            _ -> throwErr pos "Incorrect comparison operator"
    else if t1 == t2
        then return TBool
    else throwErr pos "Comparison on different types"

checkExpr (EAnd pos e1 e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if t1 == TBool && t2 == TBool
        then return TBool
    else throwErr pos "And on non-booleans"

checkExpr (EOr pos e1 e2) = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    if t1 == TBool && t2 == TBool
        then return TBool
    else throwErr pos "Or on non-booleans"

checkExpr (ETuple _ es) = do
    ts <- mapM checkExpr es
    return $ TTuple ts

checkProgram :: Program -> TCM ()
checkProgram (Program pos topDefs) = do
    mapM_ addTopDef topDefs
    setType "printInt" $ TFun TInt [TInt] $ Block pos []
    setType "printString" $ TFun TStr [TStr] $ Block pos []
    setType "readInt" $ TFun TInt [] $ Block pos []
    setType "readString" $ TFun TStr [] $ Block pos []
    setType "error" $ TFun TStr [] $ Block pos []
    let mainExists = any (\(FnDef _ retType (Ident f) args _) -> f == "main" && convertType retType == TInt && args == []) topDefs
    if not mainExists
        then throwErr pos "No main function"
    else do
        mapM_ checkFunc topDefs

typeCheck :: Program -> Either ErrMsg ()
typeCheck p = evalStateT (checkProgram p) $ TCState Map.empty TInt