data Term = Z
          | S Term
          deriving (Show, Eq)
addoIOO :: MonadPlus m => Term -> m Term -> m (Term, Term)
addoIOO x addoIOO_z = 
    msum [
        do {
            guard (x == Z);
            (y, z) <- do { x0 <- addoIOO_z; return (x0, x0); } 
            return (y, z)
        },
        do {
            S x' <- return x;
            (y, z') <- delay (addoIOO x' addoIOO_z);
            z <- return (S z');
            return (y, z)
        }
    ]
