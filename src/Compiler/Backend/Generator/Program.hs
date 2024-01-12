module Backend.Generator.Program where

import System.IO

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.DList as DList
import Data.List (isPrefixOf, isSuffixOf, intercalate, sortBy, sortOn)
import Data.Function (on)

import Latte.Abs

import Common
import Backend.Types
import Backend.Utils
import Backend.Generator.Statements (genBlock, genInit')

indentLine :: String -> String
indentLine line =
  if null line ||
     any (`isPrefixOf` line) ["@", "declare", "define"]  ||
     any (`isSuffixOf` line) ["}", ":"]
  then line ++ "\n"
  else "\t" ++ line ++ "\n"

indent :: Code -> String
indent = DList.foldr ((++) . indentLine) ""

declarations :: Code
declarations = DList.fromList [
  "declare void @printInt(i32)",
  "declare void @printString(i8*)",
  "declare void @error()",
  "declare i32 @readInt()",
  "declare i8* @readString()",
  "declare i8* @_concatStrings(i8*, i8*)",
  "declare i8* @_malloc(i32)",
  "declare void @_clearNElems(i8*, i32, i32)"
  ]

genParam :: Arg -> String
genParam (AArg _ tt (Ident id)) = genTypedVal $ 
  VLocal (T (convType tt), (StrSym id))

genStoreParam :: Arg -> CM Code
genStoreParam (AArg _ tt id@(Ident s)) = do
  let t = convType tt
  let v = VLocal (T t, StrSym s)
  genInit' t id v

genStoreParams :: [Arg] -> Code -> CM Code
genStoreParams [] acc = return acc
genStoreParams (a:as) acc = do
  code <- genStoreParam a
  genStoreParams as (DList.append acc code)

genFunc :: TType -> Ident -> [Arg] -> Block -> CM Code
genFunc tt (Ident id) as b = do
  let t = convType tt
  modify (\(locals, globals, (_, g, _), _) -> 
          (locals, globals, (0, g, 0), (t)))
  let ps = intercalate ", " (map genParam as)
  let open = DList.singleton $ "define " ++ (genType t) ++ " " ++ 
            (genGlobSymbol (StrSym id)) ++ "(" ++ ps ++ ") {"
  storeParams <- genStoreParams as DList.empty
  (code, hasRet) <- genBlock b
  let ret | (not hasRet) = DList.singleton $ "ret " ++ (genTypedDefaultVal t)
          | otherwise = DList.empty
  return $ DList.concat [open, storeParams, code, ret, DList.singleton "}"]

attributesTypes :: Ident -> CM [Type]
attributesTypes classId = do
  as <- getAttributes classId
  return $ map snd $ sortBy (compare `on` fst) (Map.elems as)

genMethod :: Ident -> CMember -> CM Code
genMethod id@(Ident classId) m = case m of
  CAttr _ _ _ -> return DList.empty
  CMethod _ tt (Ident methodId) as b -> do
    code <- genFunc tt (Ident (restrictName (classId ++ "_" ++ methodId))) 
                    ((AArg Nothing (TClass Nothing id) 
                    (Ident $ restrictName "this")):as) b
    return $ DList.append (DList.singleton "") code

genMethods :: Ident -> [CMember] -> Code -> CM Code
genMethods _ [] acc = return acc
genMethods classId (m:ms) acc = do
  code <- genMethod classId m
  genMethods classId ms (DList.append acc code)

genTypeDecl :: Ident -> [Type] -> Code
genTypeDecl id types = DList.append (DList.singleton $
  (genClassType id) ++ " = type {" ++ genTypesList types ++ "}") 
  (DList.singleton "")

genVtableTypeDecl :: Ident -> CM Code
genVtableTypeDecl classId = do
  methods <- getMethods classId
  let sorted = sortOn (\(num, _, _) -> num) (Map.elems methods)
  let types = map (\(_, id, (retT, argTs)) -> (retT, (ClassT id):argTs)) sorted
  let code = DList.singleton $ (genClassType $ vtableTypeId classId) ++ 
            " = type {" ++ genFuncTypesList types ++ "}"
  return $ DList.append code (DList.singleton "")

genVtableMethods :: [(Ident, (Int, Ident, FuncType))] -> Code -> CM Code
genVtableMethods [] acc = return acc
genVtableMethods (((Ident id), (_, classId, (retT, argTs))):ms) acc = do
  let sep | null ms = ""
          | otherwise = ","
  let code = DList.singleton $ genFuncType (retT, (ClassT classId):argTs) ++ 
            " " ++ genGlobSymbol (StrSym $ restrictName 
            (genIdent classId ++ "_" ++ id)) ++ sep
  genVtableMethods ms (DList.append acc code)

genVtableDataDecl :: Ident -> CM Code
genVtableDataDecl classId = do
  let decl = DList.singleton $ genVtableData classId ++ " = global " ++
            genVtableType classId ++ " {"
  methods <- getMethods classId
  let sorted = sortOn ((\(num, _, _) -> num) . snd) (Map.toList methods)
  code <- genVtableMethods sorted DList.empty
  return $ DList.concat [decl, code, DList.singleton "}", DList.singleton ""]

mallocObject :: Ident -> Code
mallocObject id = DList.fromList [
  "define " ++ genType (ClassT id) ++ " @" ++ 
  restrictName (genIdent id ++ "_" ++ restrictName "malloc") ++ "() {",
  "%sizeptr = getelementptr " ++ genClassType id ++ ", " ++ 
  genType (ClassT id) ++ " " ++ genVal (VConst CNull) ++ ", " ++ 
  genType IntT ++ " 1",
  "%size = ptrtoint " ++ genType (ClassT id) ++ " %sizeptr to " ++ 
  genType IntT,
  "%ptr = call " ++ genType PtrT ++ " @" ++ restrictName "malloc" ++
  "(" ++ genType IntT ++ " %size)",
  "%res = bitcast " ++ genType PtrT ++ " %ptr to " ++ genType (ClassT id),
  "ret " ++ genType (ClassT id) ++ " %res",
  "}"
  ]

genClass :: Ident -> ClassBlock -> CM Code
genClass id (CBlock _ members) = do
  types <- attributesTypes id
  let decl = genTypeDecl id ((ClassT $ vtableTypeId id):types)
  vTypeDecl <- genVtableTypeDecl id
  vDataDecl <- genVtableDataDecl id
  let malloc = mallocObject id
  methods <- genMethods id members DList.empty
  return $ DList.concat [
    decl, vTypeDecl, vDataDecl, malloc, methods
    ]

genTopDefs :: [TopDef] -> Code -> Code -> CM Code
genTopDefs [] cs fs = return $ DList.append cs fs
genTopDefs (d:ds) cs fs = case d of
  FnDef _ tt id as b -> do
    code <- genFunc tt id as b
    genTopDefs ds cs (DList.concat [fs, (DList.singleton ""), code])
  ClassDef _ id cb -> do
    code <- genClass id cb
    genTopDefs ds (DList.concat [cs, (DList.singleton ""), code]) fs
  SubclassDef _ id _ cb -> do
    code <- genClass id cb
    genTopDefs ds (DList.concat [cs, (DList.singleton ""), code]) fs

genProgr :: Program -> CM Code
genProgr (Progr _ ds) = do
  let arrayId = Ident $ restrictName "array"
  let arrayDecl = genTypeDecl arrayId [PtrT, IntT]
  let arrayMalloc = mallocObject arrayId
  code <- genTopDefs ds DList.empty DList.empty
  return $ DList.concat [DList.singleton "", arrayDecl, arrayMalloc, code]

compile :: Program -> (FuncEnv, ClassEnv) -> IO String
compile p env = do
  let initState = (Map.empty, Map.empty, (0, 0, 0), (VoidT))
  let res = runExcept $ runReaderT (runStateT (genProgr p) initState) env
  case res of
    Left err -> printError ("compilation error " ++ err)
    Right (code, (_, globals, _, _)) -> do
      let strings | Map.null globals = DList.empty
                  | otherwise = DList.append (DList.singleton "")
                    (DList.fromList $ genGlobStrings globals) 
      return $ indent $ DList.concat [declarations, strings, code]
      