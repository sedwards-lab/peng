module Ast where

import Prettyprinter
import Prelude hiding ( (<>), id )
import Duration

data Program = Program [Declaration]

data Declaration = Function String [Bind] Expr

data Bind = Bind [String] Ty

data Ty = TCon String
        | TApp Ty Ty

data Expr = Id String
          | IntLit Integer
          | StringLit String
          | DurLit Duration
          | Apply Expr Expr
          | OpRegion Expr OpRegion
          | NoExpr
          | Let [Def]
          | While Expr Expr
          | Loop Expr
          | Par [Expr]
          | IfElse Expr Expr Expr
          | Later Expr Pat Expr
          | Assign Pat Expr
          | Constraint Expr Ty
          | As String Expr
          | Wait [String]
          | Seq Expr Expr
          | Wildcard

data OpRegion = EOR
              | NextOp String Expr OpRegion

data Def = Def Pat Expr

data Pat = PId String
         | PInt Integer
         | PString String
         | PDur Duration
         | PWildcard
         | PAs String Pat
         | PCon String [Pat]

rewrite :: (Expr -> Expr) -> Expr -> Expr
rewrite f (Apply e1 e2) = Apply (f e1) (f e2)
rewrite f (OpRegion e r) = OpRegion (f e) (h r)
  where h EOR = EOR
        h (NextOp op e' r') = NextOp op (f e') (h r')
rewrite f (Let d) = Let $ map (\(Def p e) -> Def p (f e)) d
rewrite f (While e1 e2) = While (f e1) (f e2)
rewrite f (Loop e) = Loop (f e)
rewrite f (Par e) = Par $ map f e
rewrite f (IfElse e1 e2 e3) = IfElse (f e1) (f e2) (f e3)
rewrite f (Later e1 p e2) = Later (f e1) p (f e2)
rewrite f (Assign p e) = Assign p (f e)
rewrite f (Constraint e t) = Constraint (f e) t
rewrite f (As s e) = As s (f e)
rewrite f (Seq e1 e2) = Seq (f e1) (f e2)
rewrite _ e = e

instance Show Program where
  show (Program decls) = concatMap (\d -> show (pretty d) ++ "\n\n") decls

instance Show Expr where
  show e = show $ pretty e

instance Show Def where
  show d = show $ pretty d

instance Pretty Declaration where
  pretty (Function id formals body) =
    nest 2 (vsep [ pretty id <> tupled (map pretty formals) <+> pretty '='
                 , pretty body ])

instance Pretty Bind where
  pretty (Bind ids t) = hsep (punctuate comma $ map pretty ids) <+>
                        pretty ':' <+> pretty t

instance Pretty Ty where
  pretty (TCon id) = pretty id
  pretty (TApp t (TCon id)) = pretty t <+> pretty id
  pretty (TApp t1 t2) = pretty t1 <+> parens (pretty t2)

instance Pretty Expr where
  pretty (Id id) = pretty id
  pretty (IntLit i) = pretty i
  pretty (StringLit s) = pretty '"' <> pretty s <> pretty '"'
  pretty (DurLit d) = pretty $ show d
  pretty (Apply (Id id) e) = pretty id <+> pretty e
  pretty (Apply e1 e2) = parens (pretty e1) <+> pretty e2
  pretty (OpRegion e1 r) = parens (pretty e1 <> p r)
    where p EOR = emptyDoc
          p (NextOp s e r') = space <> pretty s <+> pretty e <> p r'
  pretty (As v e) = pretty v <> pretty '@' <> pretty e
  pretty NoExpr = emptyDoc
  pretty (Let defs) = pretty "let" <+> (align $ vsep $ map pretty defs)
  pretty (While e1 e2) = nest 2 $ vsep [ pretty "while" <+> pretty e1, pretty e2 ]
  pretty (Loop e) = nest 2 $ vsep [ pretty "loop", pretty e ]
  pretty (Par es) = nest 2 $ vsep $ pretty "par" : map pretty es
  pretty (IfElse e1 e2 NoExpr) = nest 2 $ vsep [ pretty "if" <+> pretty e1
                                              , pretty e2 ]
  pretty (IfElse e1 e2 e3) = vsep [ nest 2 $ vsep [ pretty "if" <+> pretty e1
                                                  , pretty e2 ]
                                  , nest 2 $ vsep [ pretty "else"
                                                  , pretty e3 ] ]
  pretty (Later e1 v e2) = pretty e1 <+> pretty "later" <+> 
                           pretty v <+> pretty "<-" <+> pretty e2
  pretty (Assign v e) = pretty v <+> pretty "<-" <+> pretty e
  pretty (Wait vars) =
      pretty "wait" <+> hsep (punctuate comma $ map pretty vars)
  pretty (Constraint e t) = pretty e <+> pretty ':' <+> pretty t
  pretty (Seq e1 e2) = vsep [pretty e1, pretty e2]
  pretty Wildcard = pretty '_'

instance Pretty Def where
  pretty (Def p e) = pretty p <+> pretty '=' <+> pretty e

instance Pretty Pat where
  pretty (PId s) = pretty s
  pretty (PInt i) = pretty i
  pretty (PString s) = pretty '"' <> pretty s <> pretty '"'
  pretty (PDur d) = pretty $ show d
  pretty PWildcard = pretty '_'
  pretty (PAs v p) = pretty v <> pretty '@' <> pretty p
  pretty (PCon c ps) = pretty c <+> hsep (map pretty ps)
