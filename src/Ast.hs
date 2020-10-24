module Ast where

import Prettyprinter
import Prelude hiding ( (<>), id )

data Program = Program [Declaration]

data Declaration = Function String [Bind] Expr

data Bind = Bind [String] Ty

data Ty = TCon String
        | TApp Ty Ty

data Expr = Id String
          | IntLit Integer
          | StringLit String
          | Apply Expr Expr
          | BinOp Expr String Expr
          | NoExpr
          | Let [Def]
          | While Expr Expr
          | Loop Expr
          | IfElse Expr Expr Expr
          | Seq Expr Expr

data Def = Def String Expr


instance Show Program where
  show (Program decls) = concatMap (\d -> show (pretty d) ++ "\n\n") decls

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
  pretty (TApp t1 t2) = parens (pretty t1) <+> pretty t2

instance Pretty Expr where
  pretty (Id id) = pretty id
  pretty (IntLit i) = pretty i
  pretty (StringLit s) = pretty '"' <> pretty s <> pretty '"'
  pretty (Apply (Id id) e) = pretty id <+> pretty e
  pretty (Apply e1 e2) = parens (pretty e1) <+> pretty e2
  pretty (BinOp e1 op e2) =
    parens (pretty e1) <+> pretty op <+> parens (pretty e2)
  pretty NoExpr = emptyDoc
  pretty (Let defs) = pretty "let" <+> (align $ vsep $ map pretty defs)
  pretty (While e1 e2) = nest 2 $ vsep [ pretty "while" <+> pretty e1, pretty e2 ]
  pretty (Loop e) = nest 2 $ vsep [ pretty "loop", pretty e ]
  pretty (IfElse e1 e2 NoExpr) = nest 2 $ vsep [ pretty "if" <+> pretty e1
                                              , pretty e2 ]
  pretty (IfElse e1 e2 e3) = vsep [ nest 2 $ vsep [ pretty "if" <+> pretty e1
                                                  , pretty e2 ]
                                  , nest 2 $ vsep [ pretty "else"
                                                  , pretty e3 ] ]
  pretty (Seq e1 e2) = vsep [pretty e1, pretty e2]

instance Pretty Def where
  pretty (Def id e) = pretty id <+> pretty '+' <+> pretty e
