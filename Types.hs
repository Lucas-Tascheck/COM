module Types where

type Id = String
data Tipo = TDouble | TFloat | TInt | TString | TVoid deriving (Eq, Show) --
data TCons = CDouble Double | CFloat Double | CInt Integer deriving Show
data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Neg Expr | Const TCons | IdVar String | Chamada Id [Expr] | Lit String | IntDouble Expr | DoubleInt Expr  deriving Show
data ExprR = Req Expr Expr | Rdif Expr Expr | Rlt Expr Expr | Rgt Expr Expr | Rle Expr Expr | Rge Expr Expr deriving Show
data ExprL = And ExprL ExprL | Or ExprL ExprL | Not ExprL | Rel ExprR deriving Show
data Var = Id :#: Tipo deriving Show --
data Funcao = Id :->: ([Var], Tipo) deriving Show  --
type Bloco = [Comando]
data Programa = Prog [Funcao] [(Id, [Var], [Comando])] [Var] [Comando] deriving Show --
data Comando = If ExprL Bloco Bloco | While ExprL Bloco | For (Id, Expr) ExprL (Id, Expr) Bloco | Atrib Id Expr | Leitura Id | Imp Expr | Ret (Maybe Expr) | Proc Id [Expr] deriving Show
