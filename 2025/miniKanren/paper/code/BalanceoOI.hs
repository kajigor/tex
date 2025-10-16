-- Monolithic term type
data Term
    = Cons Term Term
    | Leaf
    | Nil
    | Node Term Term Term
    | S Term
    | Z
    deriving (Show, Eq)

-- Explicit generator threading
balanceoOO @{\color{red}balancedoO\_x2 depthoOI\_x3}@ = msum [do {x1 <- delay (balancedoO balancedoO_x2 depthoOI_x3);
                        -- Deterministic operation, but the computation is forced to use Stream
                                                 x2 <- @{\color{red}delay}@ (traversoIO x1); 
                                                 x0 <- delay (traversoOI x2);
                                                 return (x0, x1)}]
balancedoO balancedoO_x2 depthoOI_x3 = msum [do {x0 <- return Leaf;
                                                 return x0},
                                             do {x1 <- delay (balancedoO balancedoO_x2 depthoOI_x3);
                                                 x4 <- delay (depthoIO x1);
                                                 x5 <- delay (similaroIO x4);
                                                 x3 <- delay (depthoOI x5 depthoOI_x3);
                                                 delay (balancedoI x3);
                        -- Generator for a particular variable
                                                 x2 <- @{\color{red}balancedoO\_x2}@; 
                                                 x0 <- return (Node x1 x2 x3);
                                                 return x0}]


...
