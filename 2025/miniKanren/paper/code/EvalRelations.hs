data Nat = Z | S Nat
data SimpleType = Integer | Boolean

data Lang = BConst_ Bool
          | IConst_ Nat
          | Var_ Nat
          | Plus_ Lang Lang
          | Mult_ Lang Lang
          | Eq_ Lang Lang
          | Lt_ Lang Lang
          | Let_ Lang Lang
          | If_ Lang Lang Lang

add :: (Kanren rel) => L Nat rel -> L Nat rel -> L Nat rel -> Relation rel
add = relation3 "add" $ \x y z -> conde
    [ do
        x <-> Z
        y <=> z
        pure ()
    , fresh2 $ \x' z' -> do
        x <=> suc x'
        z <=> suc z'
        call $ add x' y z'
        pure ()
    ]

leo :: (Kanren rel) => L Nat rel -> L Nat rel -> L Bool rel -> Relation rel
leo = relation3 "leo" $ \x y r -> conde
    [ x <-> Z *> r <=> true
    , fresh $ \zz -> x <=> suc zz *> y <-> Z *> r <=> false
    , fresh2 $ \x' y' -> do
        x <=> suc x'
        y <=> suc y'
        call $ leo x' y' r
        pure ()
    ]

gto :: (Kanren rel) => L Nat rel -> L Nat rel -> L Bool rel -> Relation rel
gto = relation3 "gto" $ \x y r -> conde
    [ fresh $ \zz -> x <=> suc zz *> y <-> Z *> r <=> true
    , x <-> Z *> r <=> false
    , fresh2 $ \x' y' -> do
        x <=> suc x'
        y <=> suc y'
        call $ gto x' y' r
        pure ()
    ]

minmaxo :: (Kanren rel) => L Nat rel -> L Nat rel -> L Nat rel -> L Nat rel -> Relation rel
minmaxo = relation4 "minmaxo" $ \a b min max -> conde
    [ min <=> a *> max <=> b *> (call $ leo a b true)
    , min <=> b *> max <=> a *> (call $ gto a b true)
    ]

smallesto :: (Kanren rel) => L [Nat] rel -> L Nat rel -> L [Nat] rel -> Relation rel
smallesto = relation3 "smallesto" $ \l s l' -> conde
    [ l <=> cons s nil *> l' <-> []
    , fresh5 $ \h t s' t' max -> do
        l' <=> cons max t'
        l <=> cons h t
        call $ minmaxo h s' s max
        call $ smallesto t s' t'
        pure ()
    ]

sorto :: (Kanren rel) => L [Nat] rel -> L [Nat] rel -> Relation rel
sorto = relation2 "sorto" $ \x y -> conde
    [ x <-> [] *> y <-> []
    , fresh3 $ \s xs xs' -> do
        y <=> cons s xs'
        call $ sorto xs xs'
        call $ smallesto x s xs'
        pure ()
    ]

idx :: (Kanren rel, LogicType a) => L Nat rel -> L [a] rel -> L (Maybe a) rel -> Relation rel
idx = relation3 "idx" $ \k xs v -> fresh3 $ \k1 t h -> conde
    [ xs <-> [] *> v <-> Nothing
    , k <-> Z *> xs <=> cons h t *> v <=> some h
    , k <=> suc k1 *> xs <=> cons h t *> call (idx k1 t v)
    ]

isBool :: (Kanren rel) => L Bool rel -> Relation rel
isBool = relation "isBool" $ \x -> conde
    [ x <=> true
    , x <=> false
    ]

isNat :: (Kanren rel) => L Nat rel -> Relation rel
isNat = relation "isNat" $ \x -> conde
    [ x <=> zro
    , fresh $ \y -> x <=> suc y *> call (isNat y)
    ]

type_ :: (Kanren rel) => L Lang rel -> L [SimpleType] rel -> L (Maybe SimpleType) rel -> Relation rel
type_ = relation3 "type_" $ \term gamma ttype -> 
    fresh3 $ \els thn cond ->
        fresh2 $ \btype1 btype ->
            fresh2 $ \body bound ->
                fresh2 $ \t1 t ->
                    fresh4 $ \r l n m ->
                        fresh3 $ \v y x ->
                            conde 
                            [ term <=> bconst x *> call (isBool x) *> ttype <=> bool
                            , term <=> iconst y *> call (isNat y) *> ttype <=> int
                            , term <=> var v *> call (isNat v) *> call (idx v gamma ttype)
                            , do
                                term <=> plus n m
                                call $ type_ m gamma int
                                call $ type_ n gamma int
                                ttype <=> int
                                pure ()
                            , do
                                term <=> mult n m
                                call $ type_ m gamma int
                                call $ type_ n gamma int
                                ttype <=> int
                                pure ()
                            , do
                                term <=> eq l r
                                call $ type_ l gamma t
                                call $ type_ r gamma t
                                ttype <=> bool
                                t <=> some t1
                                pure ()
                            , do
                                term <=> lt l r
                                call $ type_ l gamma int
                                call $ type_ r gamma int
                                ttype <=> bool
                                pure ()
                            , do
                                term <=> let_ bound body
                                call $ type_ bound gamma btype
                                btype <=> some btype1
                                call $ type_ body (cons btype1 gamma) ttype
                                pure ()
                            , do
                                term <=> if_ cond thn els
                                call $ type_ cond gamma bool
                                call $ type_ thn gamma ttype
                                call $ type_ els gamma ttype
                                pure ()
                            ]
