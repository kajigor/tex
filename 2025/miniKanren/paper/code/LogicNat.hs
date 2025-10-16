data Nat = Z | S Nat deriving (Eq, Ord, Show)
instance LogicType Nat where
    data WithLogic Nat var = Z' | S' (Logic Nat var)

    quote Z' = quote0 "Z" Z'
    quote (S' n) = quote1 "S" S' n

    unifyVal _ Z' Z' = pure ()
    unifyVal unif (S' x) (S' y) = unif x y
    unifyVal _ _ _ = empty

    derefVal _ Z' = pure Z
    derefVal deref (S' n) = S <$> (deref n)

    project Z = Z'
    project (S n) = S' $ Ground (project n)

    reify Z' = Just Z
    reify (S' (Ground n)) = S <$> reify n
    reify _ = Nothing

    generate = pure Z <> (S <$> generate)
