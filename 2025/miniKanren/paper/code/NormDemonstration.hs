add :: (Kanren rel) => 
       Logic Nat (KVar rel) -> Logic Nat (KVar rel) -> Logic Nat (KVar rel)
    -> (String, rel ())
add = ...

addNorm :: (Kanren rel) => 
           Logic Nat (KVar rel) -> Logic Nat (KVar rel) -> Logic Nat (KVar rel) 
        -> (String, rel ())
addNorm x y z = normalize <$> add x y z 
-- add is instantiated with NormalizedKanrenT rel, and then interpreted back to rel
