module Ast2IR ( astToIR ) where

import qualified Ast as A
import qualified IR as I
import Data.Maybe ( mapMaybe )
import Prelude hiding ( id, lookup )
import Control.Monad.State

type SymbolTable = [I.Bind]

data IRGenState = IRGenState { nextLabel :: I.Lab
                             , symbolTable :: SymbolTable
                             , generatedStatements :: [I.Statement]
                             }

initialState :: SymbolTable -> IRGenState
initialState env = IRGenState { nextLabel = I.initialLabel
                              , symbolTable = env
                              , generatedStatements = []
                              }

type IRGenMonad = State IRGenState

freshLabel :: IRGenMonad I.Lab
freshLabel = do st <- get
                let lab = nextLabel st
                put $ st { nextLabel = I.nextLabel lab }
                return lab

lookup :: String -> IRGenMonad I.Bind
lookup n = do symbols <- gets symbolTable
              return $ lhelper symbols
  where lhelper [] = error $ "Undefined symbol " ++ n
        lhelper (b@(I.Bind n' _):_) | n' == n = b
        lhelper (_:syms) = lhelper syms              

emit :: I.Statement -> IRGenMonad ()
emit stmt = do st <- get
               put $ st { generatedStatements =
               stmt : generatedStatements st }

generateStatements :: SymbolTable -> IRGenMonad () -> [I.Statement]
generateStatements env f =
  reverse $ generatedStatements $ execState f (initialState env)

astToIR :: A.Program -> I.Program
astToIR (A.Program decls) = I.Program functions
  where    
    functions = mapMaybe convertFunction decls

    topDefs =
      I.Bind "Occur" (I.TCon "Event") :  -- FIXME: This is really ugly
      mapMaybe bindOfDecl decls

    bindOfDecl decl@(A.Function name _ _) = Just $ I.Bind name $ typeOfDecl decl
    bindOfDecl _ = Nothing
    
    typeOfDecl (A.Function _ formals _) =
      I.functionType (formalTypes ++ [resultType])
      where
        resultType = I.unitTy -- FIXME: the default for now
        formalTypes = concatMap bindTypes formals
        bindTypes (A.Bind ids t) = map (\_ -> t') ids
          where t' = convertTy t
    typeOfDecl _ = error "typeOfDecl called on a non-function"

    convertFunction decl@(A.Function fname formalArgs body) =
      Just $ I.Function bind formals locals body'
      where
        functionType = typeOfDecl decl
        bind = I.Bind fname functionType
        formals = zipWith I.Bind formalNames $ I.functionArgsTypes functionType
        formalNames = concatMap (\(A.Bind ids _) -> ids) formalArgs

        locals = collectBinders body
        environment = locals ++ formals ++ topDefs
        body' = generateStatements environment $ emitStmt body

    convertFunction _ = Nothing        
    
    convertBind :: A.Bind -> [I.Bind]
    convertBind (A.Bind ids t) = map (\id -> I.Bind id t') ids
      where
        t' = convertTy t

    convertTy (A.TCon t) = I.TCon t
    convertTy (A.TApp t1 t2) = I.TApp (convertTy t1) (convertTy t2)

    emitStmt :: A.Expr -> IRGenMonad ()
--    emitStmt (A.Binding _) = return ()
    emitStmt (A.Seq e1 e2) = emitStmt e1 >> emitStmt e2
{-    emitStmt (A.Assign id e) = do bind <- lookup id
                                  e' <- toExpr e
                                  emit $ I.Assign bind e' -}
    emitStmt (A.Loop e) = do lab <- freshLabel
                             emit $ I.Label lab
                             emitStmt e
                             emit $ I.Goto lab
    emitStmt (A.While p e) = do
      firstLab <- freshLabel
      lastLab <- freshLabel
      emit $ I.Label firstLab
      p' <- toExpr p
      emit $ I.IfGoto (I.Unary I.Not p' I.boolTy) lastLab
      emitStmt e
      emit $ I.Goto firstLab
      emit $ I.Label lastLab
    emitStmt (A.IfElse e thenStmt A.NoExpr) = do
      lab <- freshLabel
      e' <- toExpr e
      emit $ I.IfGoto (I.Unary I.Not e' I.boolTy) lab
      emitStmt thenStmt
      emit $ I.Label lab
      
    emitStmt (A.IfElse e thenStmt elseStmt) = do
      elseLabel <- freshLabel
      contLabel <- freshLabel
      e' <- toExpr e      
      emit $ I.IfGoto (I.Unary I.Not e' I.boolTy) elseLabel
      emitStmt thenStmt
      emit $ I.Goto contLabel      
      emit $ I.Label elseLabel
      emitStmt elseStmt
      emit $ I.Label contLabel
      
{-    emitStmt (A.After e1 id e2) = do bind <- lookup id
                                     e2' <- toExpr e2
                                     e1' <- toExpr e1
                                     emit $ I.After e1' bind e2'
    emitStmt (A.Wait ids) = do binds <- mapM lookup ids
                               emit $ I.Wait binds
    emitStmt (A.Call f args) = do args' <- mapM toExpr args
                                  f' <- lookup f
                                  emit $ I.Fork [(f', args')]
    emitStmt (A.Verbatim s) = emit $ I.Verbatim s -}

{-    emitStmt calls@(A.BinOp _ "||" (A.Call _ _)) = do
      let calls' = reverse $ collectCalls calls
      as <- mapM (mapM toExpr) (map snd calls')
      fs <- mapM lookup (map fst calls')
      emit $ I.Fork $ zip fs as       -}
      
    emitStmt A.NoExpr = return ()
    emitStmt e = error $ "Unsupported statement expression " ++ show e

    collectCalls _ = []
{-
    collectCalls (A.Call f args) = [(f, args)]
    collectCalls (A.BinOp calls "||" (A.Call f1 a1)) =
      (f1, a1) : collectCalls calls
    collectCalls e = error $ "Unexpected call expression " ++ show e
-}

    toExpr :: A.Expr -> IRGenMonad I.Expression
    toExpr (A.IntLit i) = return $ I.IntLit i I.intTy
    toExpr (A.DurLit d) = return $ I.DurLit d I.timeTy
    toExpr (A.Id v) = I.Var <$> lookup v
    toExpr (A.BinOp e1 op e2) = do
      e1' <- toExpr e1
      e2' <- toExpr e2
      let (op', ty) = case op of
            "+" -> (I.Add, I.intTy)
            "-" -> (I.Sub, I.intTy)
            "*" -> (I.Mult, I.intTy)
            "<" -> (I.LT, I.boolTy)
            "<=" -> (I.LE, I.boolTy)
            ">" -> (I.GT, I.boolTy)
            ">=" -> (I.GE, I.boolTy)
            "/=" -> (I.NE, I.boolTy)
            _   -> error $ "unsupported binary operator " ++ op
      return $ I.Binary e1' op' e2' ty
           
    toExpr e = error $ "Unsupported expression " ++ show e

    collectBinders :: A.Expr -> [I.Bind]
    collectBinders _ = []
{-    
    collectBinders (A.Binding b) = convertBind b
    collectBinders (A.Sequence e1 e2) = collectBinders e1 ++ collectBinders e2
    collectBinders (A.Assign _ e) = collectBinders e
    collectBinders (A.After e1 _ e2) = collectBinders e1 ++ collectBinders e2
    collectBinders (A.Loop e) = collectBinders e
    collectBinders (A.While p e) = collectBinders p ++ collectBinders e
    collectBinders (A.IfThenElse e1 e2 e3) = concatMap collectBinders [e1,e2,e3]
    collectBinders A.Wait{} = []
    collectBinders A.IntLit{} = []
    collectBinders A.TimeLit{} = []
    collectBinders A.Id{} = []
    collectBinders (A.Call _ as) = concatMap collectBinders as
    collectBinders (A.BinOp e1 _ e2) = collectBinders e1 ++ collectBinders e2
    collectBinders A.Verbatim{} = []
    collectBinders A.NoExpr = []
-}
