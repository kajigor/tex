balanceo :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> L (Tree elem) rel -> Relation rel
balanceo = relation2 "balanceo" $ \v u -> fresh $ \e -> do
    call $ traverso v e
    call $ traverso u e
    call $ balancedo u
    pure ()

traverso :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> L [elem] rel -> Relation rel
traverso = relation2 "traverso" $ \t e -> conde 
    [ do
        t <=> leaf
        e <=> nil
        pure ()
    , fresh5 $ \l x r el er -> do
        t <=> node l x r
        call $ traverso l el
        call $ traverso r er
        call $ appendo el (cons x er) e
        pure ()
    ]

appendo :: (Kanren rel, LogicType elem) => L [elem] rel -> L [elem] rel -> L [elem] rel -> Relation rel
appendo = relation3 "appendo" $ \x y xy -> conde
    [ do
        x <=> nil
        y <=> xy
        pure ()
    , fresh3 $ \h x' xy' -> do
        x <=> cons h x'
        xy <=> cons h xy'
        call $ appendo x' y xy'
        pure ()
    ]

balancedo :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> Relation rel
balancedo = relation "balancedo" $ \t -> conde 
    [ t <=> leaf
    , fresh5 $ \l x r dl dr -> do
        t <=> node l x r
        call $ deptho l dl
        call $ deptho r dr
        call $ similaro dl dr
        call $ balancedo l
        call $ balancedo r
        pure ()
    ]

similaro :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
similaro = relation2 "similaro" $ \x y -> conde [ x <=> y, x <=> suc y, y <=> suc x ]

deptho :: (Kanren rel, LogicType elem) => L (Tree elem) rel -> L Nat rel -> Relation rel
deptho = relation2 "deptho" $ \t d -> conde 
    [ do
        t <=> leaf
        d <=> zro
        pure ()
    , fresh3 $ \l x r -> fresh3 $ \ld rd d' -> do
        t <=> node l x r
        d <=> suc d'
        call $ deptho l ld
        call $ deptho r rd
        call $ maxo ld rd d'
        pure ()
    ]

lto :: (Kanren rel) => L Nat rel -> L Nat rel -> Relation rel
lto = relation2 "lto" $ \l g -> conde 
    [ fresh $ \g' -> do
        l <=> zro
        g <=> suc g'
        pure ()
    , fresh2 $ \l' g' -> do
        l <=> suc l'
        g <=> suc g'
        call $ lto l' g'
        pure ()
    ]

maxo :: (Kanren rel) => L Nat rel -> L Nat rel -> L Nat rel -> Relation rel
maxo = relation3 "minmaxo" $ \x y mx -> conde
    [ mx <=> y *> call $ x `lto` (suc y)
    , mx <=> x *> call $ y `lto` x
    ]


