module Types where

type Id = String
data Tipo = TDouble | TInt | TString | TVoid deriving (Eq, Show) --
data TCons = CDouble Double | CInt Integer deriving Show
data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Const TCons | IdVar String | Chamada Id [Expr] | Lit String | IntDouble Expr | DoubleInt Expr  deriving Show
data ExprR = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving Show
data ExprL = ExprL :&: ExprL | ExprL :|: ExprL | Not ExprL | Rel ExprR deriving Show
data Var = Id :#: Tipo deriving Show --
data Funcao = Id :->: ([Var], Tipo) deriving Show  --
type Bloco = [Comando]
data Programa = Prog [Funcao] [(Id, [Var], [Comando])] [Var] [Comando] deriving Show --
data Comando = If ExprL Bloco Bloco | While ExprL Bloco | Atrib Id Expr | Leitura Id | Imp Expr | Ret (Maybe Expr) | Proc Id [Expr] deriving Show
