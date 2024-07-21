import Prelude hiding (elem)

data Formula
  = Lit Bool 
  | Var Int 
  | Neg Formula 
  | Conj Formula Formula 
  | Disj Formula Formula
  deriving (Show, Eq) 

elem :: [a] -> Int -> a
elem (h : t) 0 = h
elem (_ : t) n = elem t (n - 1)

eval :: [Bool] -> Formula -> Bool 
eval subst (Lit b) = b 
eval subst (Var v) = elem subst v 
eval subst (Neg z) = not $ eval subst z 
eval subst (Conj x y) = eval subst x && eval subst y 
eval subst (Disj x y) = eval subst x || eval subst y 

test f x = do 
  print x 
  print (f x)

main = do 
  test (eval [True, True]) (Conj (Neg (Var 0)) (Disj (Var 1) (Lit False)))
  test (eval [True, False]) (Conj (Neg (Var 0)) (Disj (Var 1) (Lit False)))
  test (eval [False, True]) (Conj (Neg (Var 0)) (Disj (Var 1) (Lit False)))
  test (eval [False, False]) (Conj (Neg (Var 0)) (Disj (Var 1) (Lit False)))

