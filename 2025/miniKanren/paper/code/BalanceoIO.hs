data Term
    = Cons Term Term
    | Leaf
    | Nil
    | Node Term Term Term
    | S Term
    | Z
    deriving (Show, Eq)
balanceoIO x0 depthoIO_x1 maxoIIO_x8 = msum [do {x2 <- delay (traversoIO x0);
                                                 x1 <- delay (traversoOI x2);
                                                 delay (balancedoI x1 depthoIO_x1 maxoIIO_x8);
                                                 return x1}]
balancedoI x0 depthoIO_x1 maxoIIO_x8 = msum [do {guard (x0 == Leaf);
                                                 return ()},
                                             do {Node x1 x2 x3 <- return x0;
                                                 delay (balancedoI x1 depthoIO_x1 maxoIIO_x8);
                                                 delay (balancedoI x3 depthoIO_x1 maxoIIO_x8);
                                                 x4 <- delay (depthoIO x1 depthoIO_x1 maxoIIO_x8);
                                                 x5 <- delay (depthoIO x3 depthoIO_x1 maxoIIO_x8);
                                                 delay (similaroII x4 x5);
                                                 return ()}]
depthoIO x0 depthoIO_x1 maxoIIO_x8 = msum [do {x1 <- return Z;
                                               guard (x0 == Leaf);
                                               return x1},
                                           do {Node x2 x3 x4 <- return x0;
                                               x5 <- delay (depthoIO x2 depthoIO_x1 maxoIIO_x8);
                                               x6 <- delay (depthoIO x4 depthoIO_x1 maxoIIO_x8);
                                               x7 <- delay (maxoIIO x5 x6 maxoIIO_x8);
                                               x1 <- return (S x7);
                                               return x1}]
maxoIIO x0 x1 maxoIIO_x8 = msum [do {x8 <- return (S x1);
                                     delay (ltoII x0 x8);
                                     x2 <- return x1;
                                     return x2},
                                 do {delay (ltoII x1 x0); x2 <- return x0; return x2}]
ltoII x0 x1 = msum [do {guard (x0 == Z);
                        S x2 <- return x1;
                        return ()},
                    do {S x3 <- return x0; S x4 <- return x1; ltoII x3 x4; return ()}]
similaroII x0 x1 = msum [do {guard (x0 == x1); return ()},
                         do {guard (x0 == S x1); return ()},
                         do {guard (x1 == S x0); return ()}]
traversoIO x0 = msum [do {x1 <- return Nil;
                          guard (x0 == Leaf);
                          return x1},
                      do {Node x2 x3 x4 <- return x0;
                          x5 <- traversoIO x2;
                          x6 <- traversoIO x4;
                          x9 <- return (Cons x3 x6);
                          x1 <- appendoIIO x5 x9;
                          return x1}]
appendoIIO x0 x1 = msum [do {guard (x0 == Nil);
                             x2 <- return x1;
                             return x2},
                         do {Cons x3 x4 <- return x0;
                             x5 <- appendoIIO x4 x1;
                             x2 <- return (Cons x3 x5);
                             return x2}]
traversoOI x1 = msum [do {x0 <- return Leaf;
                          guard (x1 == Nil);
                          return x0},
                      do {(x5, x9) <- delay (appendoOOI x1);
                          Cons x3 x6 <- return x9;
                          x2 <- delay (traversoOI x5);
                          x4 <- delay (traversoOI x6);
                          x0 <- return (Node x2 x3 x4);
                          return x0}]
appendoOOI x2 = msum [do {x0 <- return Nil;
                          x1 <- return x2;
                          return (x0, x1)},
                      do {Cons x3 x5 <- return x2;
                          (x4, x1) <- delay (appendoOOI x5);
                          x0 <- return (Cons x3 x4);
                          return (x0, x1)}]
