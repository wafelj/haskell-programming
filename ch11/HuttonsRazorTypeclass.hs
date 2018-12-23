data Expr
  = Lit Integer
  | Add Expr Expr

instance Show Expr where
  show (Lit i) = show i
  show (Add e1 e2) = unwords [show e1, "+", show e2]

eval :: Expr -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
