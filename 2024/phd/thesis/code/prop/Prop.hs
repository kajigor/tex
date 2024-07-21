import Prelude hiding (lookup)

data Formula
  = Lit Bool 
  | Var String 
  | Neg Formula 
  | Conj Formula Formula 
  | Disj Formula Formula
  deriving (Show, Eq) 

lookup :: Ord k => [(k, v)] -> k -> v
lookup ((k, v) : t) k1 
  | k == k1 = v 
  | otherwise = lookup t k1  

eval :: [(String, Bool)] -> Formula -> Bool 
eval subst (Lit b) = b 
eval subst (Var v) = lookup subst v 
eval subst (Neg z) = not $ eval subst z 
eval subst (Conj x y) = eval subst x && eval subst y 
eval subst (Disj x y) = eval subst x || eval subst y 

test f x = do 
  print x 
  print (f x)

main = 
  test (eval [("x", True)]) (Conj (Neg (Var "x")) (Disj (Var "x") (Lit False)))
