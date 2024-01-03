module Frontend.Typechecker.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Latte.Abs
import Latte.Print (printTree)

import Common
import Frontend.Types
import Frontend.Utils
import Frontend.Typechecker.Statements (checkBlock)

setFuncId :: Pos -> Ident -> TM Env
setFuncId pos id = do
  funcEnv <- getFuncEnv
  if Map.member id funcEnv
    then throwE pos $
      "function " ++ printTree id ++ " is already defined"
  else setFunc id PtrT []

setClassId :: Pos -> Ident -> TM Env
setClassId pos id = do
  classEnv <- getClassEnv
  if Map.member id classEnv
    then throwE pos $
      "class " ++ printTree id ++ " is already defined"
  else setClass id Map.empty Map.empty Nothing

setTopDef :: TopDef -> TM Env
setTopDef d = case d of
  FnDef pos _ id _ _ -> setFuncId pos id
  ClassDef pos id _ -> setClassId pos id
  SubclassDef pos id _ _ -> setClassId pos id

setTopDefs :: [TopDef] -> TM Env
setTopDefs [] = ask
setTopDefs (d:ds) = do
  env <- setTopDef d
  local (const env) $ setTopDefs ds

typeOfParam :: Arg -> TM Type
typeOfParam (AArg pos tt id) = do
  let t = convType tt
  isValid <- isValidType t
  if not isValid
    then throwE pos $ "invalid type " ++ showType t ++ " of function parameter"
  else return t

typeOfParams :: [Arg] -> TM [Type]
typeOfParams [] = return []
typeOfParams (p:ps) = do
  t <- typeOfParam p
  ts <- typeOfParams ps
  return (t:ts)

checkFuncSignature :: Pos -> TType -> Ident -> [Arg] -> TM Env
checkFuncSignature pos tt id ps = do
  let t = convType tt
  isValid <- isValidType t
  if (not isValid) && (t /= VoidT)
    then throwE pos $ "function returns invalid type " ++ showType t
  else do
    paramTs <- typeOfParams ps
    setFunc id t paramTs

checkAttribute :: AttrItem -> AttrEnv -> Int -> Type -> TM (AttrEnv, Int)
checkAttribute (AttrNoInit pos id) attributes num t = do
  if Map.member id attributes
    then throwE pos $
      "class attribute " ++ printTree id ++ " is already declared in this class"
  else return (Map.insert id (num, t) attributes, succ num)

checkAttributes :: [AttrItem] -> AttrEnv -> Int -> Type -> TM (AttrEnv, Int)
checkAttributes [] attributes num _ = return (attributes, num)
checkAttributes (a:as) attributes num t = do
  (attributes', num') <- checkAttribute a attributes num t
  checkAttributes as attributes' num' t

checkMembers' :: [CMember] -> Int -> AttrEnv -> MethodEnv -> 
  TM (AttrEnv, MethodEnv)
checkMembers' [] _ attributes methods = return (attributes, methods)
checkMembers' (m:ms) num attributes methods = case m of
  CAttr pos tt as -> do
    let t = convType tt
    isValid <- isValidType t
    if isValid
      then do
        (attributes', num') <- checkAttributes as attributes num t
        checkMembers' ms num' attributes' methods
    else throwE pos $
      "invalid type " ++ showType t ++ " of class attribute"
  CMethod pos tt id as _ -> do
    if Map.member id methods
      then throwE pos $ 
        "method " ++ printTree id ++ " is already defined in this class"
    else do
      let t = convType tt
      isValid <- isValidType t
      if (not isValid) && (t /= VoidT)
        then throwE pos $ "method returns invalid type " ++ showType t
      else do
        paramTs <- typeOfParams as
        checkMembers' ms num attributes (Map.insert id (t, paramTs) methods)

checkMembers :: ClassBlock -> TM (AttrEnv, MethodEnv)
checkMembers (CBlock _ ms) = checkMembers' ms 0 Map.empty Map.empty

checkClassMembers :: Ident -> ClassBlock -> Super -> TM Env
checkClassMembers id cb super = do
  (attributes, methods) <- checkMembers cb
  setClass id attributes methods super

checkTopDef1 :: TopDef -> TM Env
checkTopDef1 d = case d of
  FnDef pos tt id as _ -> checkFuncSignature pos tt id as
  ClassDef _ id cb -> checkClassMembers id cb Nothing
  SubclassDef pos id superId cb -> do
    classEnv <- getClassEnv
    if (id == superId)
      then throwE pos $ "class cannot extend itself"
    else if Map.notMember superId classEnv
      then throwE pos $ "undefined class: " ++ printTree superId
    else checkClassMembers id cb (Just superId)

checkTopDefs1 :: [TopDef] -> TM Env
checkTopDefs1 [] = ask
checkTopDefs1 (d:ds) = do
  env <- checkTopDef1 d
  local (const env) $ checkTopDefs1 ds

checkParam :: Arg -> TM Env
checkParam (AArg pos tt id) = do
  let t = convType tt
  varEnv <- getVarEnv
  if Map.member id varEnv
    then throwE pos $
      "function parameters have the same identifiers " ++ 
      printTree id
  else setVar id t

checkParams :: [Arg] -> TM Env
checkParams [] = ask
checkParams (p:ps) = do
  env <- checkParam p
  local (const env) $ checkParams ps

doesReturnOrLoopForever :: Stmt -> Bool
doesReturnOrLoopForever s = case s of
  SRet _ _ -> True
  SBlock _ (BBlock _ ss) -> doReturnOrLoopForever ss
  SExp _ (ECall _ (Ident "error") []) -> True
  SIf _ e s -> do
    case tryEval e of
      Just (CBool b) -> do
        if b == True    -- expression evaluates to True
          then doesReturnOrLoopForever s
        else False      -- expression evaluates to False
      Nothing -> False  -- could not evaluate expression 
  SIfElse _ e sIf sElse -> do
    case tryEval e of
      Just (CBool b) -> do
        if b == True
          then doesReturnOrLoopForever sIf
        else doesReturnOrLoopForever sElse
      Nothing -> (doesReturnOrLoopForever sIf) && (doesReturnOrLoopForever sElse)
  SWhile _ e s -> do
    case tryEval e of
      Just (CBool b) -> b
      Nothing -> False
  SFor _ _ _ _ s -> doesReturnOrLoopForever s
  _ -> False

doReturnOrLoopForever :: [Stmt] -> Bool
doReturnOrLoopForever [] = False
doReturnOrLoopForever (s:ss) = do
  if doesReturnOrLoopForever s
    then True
  else doReturnOrLoopForever ss

analyseFlow :: Block -> Ident -> TM ()
analyseFlow (BBlock pos ss) id = do
  if (doReturnOrLoopForever ss)
    then return ()
  else throwE pos $
    "function " ++ printTree id ++ " does not always return a value"

checkFuncBody :: Ident -> [Arg] -> Block -> Context -> TM ()
checkFuncBody id ps b context@(t, _) = do
  put (context)
  (_, funcEnv, classEnv, _) <- ask
  env <- local (const (Map.empty, funcEnv, classEnv, Set.empty)) $ 
        checkParams ps
  local (const env) $ checkBlock b
  if t /= VoidT
    then analyseFlow b id
  else return ()

checkMethods :: [CMember] -> Ident -> TM ()
checkMethods [] _ = return ()
checkMethods (m:ms) id = case m of
  CAttr _ _ _ -> checkMethods ms id
  CMethod _ _ metId as b -> do
    methods <- getMethods id
    let (t, _) = methods Map.! metId
    checkFuncBody metId as b (t, Just id)
    checkMethods ms id

checkCyclicInheritance :: Pos -> Ident -> Int -> TM ()
checkCyclicInheritance pos classId steps = do
  if steps > 0
    then do
      super <- getSuper classId
      case super of 
        Just superId -> checkCyclicInheritance pos superId (pred steps)
        Nothing -> return ()
  else throwE pos $ "cyclic inheritance hierarchy detected"

checkAttrOverride :: Pos -> Ident -> [Ident] -> TM ()
checkAttrOverride pos classId prevAttrs = do
  attributes <- getAttributes classId
  let allAttrs = List.union (Map.keys attributes) prevAttrs
  super <- getSuper classId
  case super of
    Just superId -> do
      superAttrs <- getAttributes superId
      if (any (`elem` allAttrs) (Map.keys superAttrs))
        then throwE pos $ 
          "subclass of class " ++ printTree superId ++ 
          " overrides its attribute"
      else checkAttrOverride pos superId allAttrs
    Nothing -> return ()

checkTopDef2 :: TopDef -> TM ()
checkTopDef2 d = case d of
  FnDef _ _ id as b -> do
    funcEnv <- getFuncEnv
    let (t, _) = funcEnv Map.! id
    checkFuncBody id as b (t, Nothing)
  ClassDef _ id (CBlock _ ms) -> checkMethods ms id
  SubclassDef pos id _ (CBlock _ ms) -> do
    checkCyclicInheritance pos id 100
    checkAttrOverride pos id []
    checkMethods ms id

checkTopDefs2 :: [TopDef] -> TM ()
checkTopDefs2 [] = return ()
checkTopDefs2 (d:ds) = do
  checkTopDef2 d
  checkTopDefs2 ds

checkProgr :: Program -> TM (FuncEnv, ClassEnv)
checkProgr (Progr pos ds) = do
  env <- setTopDefs ds
  env'@(_, funcEnv, classEnv, _) <- local (const env) $ checkTopDefs1 ds
  local (const env') $ checkTopDefs2 ds
  let main = Ident "main"
  if Map.notMember main funcEnv
    then throwE pos $
      "main function is not defined"
  else do
    let (retT, paramTs) = funcEnv Map.! main
    if retT /= IntT
      then throwE pos $
        "main function must be int-type"
    else if not $ null paramTs
      then throwE pos $
        "main function cannot have any parameters"
    else return (funcEnv, classEnv)

typecheck :: Program -> IO (FuncEnv, ClassEnv)
typecheck p = do
  let initVarEnv = Map.empty
  let initFuncEnv = Map.fromList [
        (Ident "printInt", (VoidT, [IntT])),
        (Ident "printString", (VoidT, [StringT])),
        (Ident "error", (VoidT, [])),
        (Ident "readInt", (IntT, [])),
        (Ident "readString", (StringT, [])),
        (Ident "_concatStrings", (StringT, [StringT, StringT])),
        (Ident "_clearNElems", (PtrT, [PtrT, IntT, IntT])),
        (Ident "_mallocArrayType", (ArrayT VoidT, []))
        ]
  let initClassEnv = Map.empty
  let initContext = (PtrT, Nothing)
  let initEnv = (initVarEnv, initFuncEnv, initClassEnv, Set.empty)
  let res = runExcept $ 
              runReaderT (
                evalStateT (checkProgr p) initContext
              ) initEnv
  case res of
    Left err -> printError ("semantic error " ++ err)
    Right env -> return env
