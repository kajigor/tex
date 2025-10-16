-- Constructors separated into underlying types
data Nat = Z | S Nat deriving (Eq, Ord, Show)
data Tree elem = Leaf | Node (Tree elem) elem (Tree elem) deriving (Eq, Ord, Show)

liftMaybe :: (Alternative m) => Maybe a -> m a
liftMaybe Nothing  = empty
liftMaybe (Just a) = pure a

-- No extra arguments
balanceoOO = msum [ do x1 <- delay balancedoO
            -- Computation converted to Maybe on a deterministic relation
                       x2 <- @{\color{blue}liftMaybe}@ (traversoIO x1) 
                       x0 <- delay (traversoOI x2)
                       return (x0, x1)]
balancedoO = msum [ do x0 <- return Leaf
                       return x0,
                    do x1 <- delay balancedoO
                       x4 <- delay (depthoIO x1)
                       x5 <- delay (similaroIO x4)
                       x3 <- delay (depthoOI x5)
                       delay (balancedoI x3)
            -- Generator is run from the inferred type
                       x2 <- @{\color{blue}generate}@ 
                       x0 <- return (Node x1 x2 x3)
                       return x0]
