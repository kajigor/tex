data Term
    = Z
    | S Term
    deriving (Show, Eq)
addoIOO x0 addoIOO_x1 = msum [
                                do {
                                    guard (x0 == Z);
                                    (x1, x2) <- runGen addoIOO_x1;
                                    return (x1, x2)
                                },
                                do {
                                    S x3 <- return x0;
                                    (x1, x4) <- delay (addoIOO x3 addoIOO_x1);
                                    x2 <- return (S x4);
                                    return (x1, x2)
                                }
                            ]
addoIOO :: Nat -> Stream (Nat, Nat)
addoIOO x0 = msum [
                    do {
                        guard (x0 == Z)
                        (x1, x2) <- runGen
                        return (x1, x2)
                    },
                    do {
                        S x3 <- return x0
                        (x1, x4) <- delay (addIOO x3)
                        x2 <- return (S x4)
                        return (x1, x2)
                    }
                ]
