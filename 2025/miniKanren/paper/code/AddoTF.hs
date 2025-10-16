add :: (Kanren rel) => Logic Nat (KVar rel) -> Logic Nat (KVar rel) -> L Nat (KVar rel) -> (String, rel ())
add = relation3 "add" $ \x y z -> conde
    [ do
        x <=> zro -- infix synonym for unify
        y <=> z
        pure () -- ApplicativeDo requires that the final statement be exactly "pure E"
    , fresh2 $ \x' z' -> do
        x <=> suc x'
        z <=> suc z'
        call $ add x' y z'
        pure ()
    ]
