data NormalizedBaseT (rel :: Type -> Type) where

    Unify :: (LogicType a) => 
             Var' a (NormalizedKanrenT rel) -> L a (NormalizedKanrenT rel) 
          -> NormalizedBaseT rel
    Call :: String -> NormalizedKanrenT rel () -> NormalizedBaseT rel

newtype NormalizedConjT rel = Conj { unConj :: [NormalizedBaseT rel] }
data NormalizedRaiseT rel a = Raise { goal :: NormalizedConjT rel, retval :: a } deriving (Functor)
newtype NormalizedDisjT rel a = Disj { unDisj :: [NormalizedRaiseT rel a] } deriving (Functor)

data NormalizedFreshT rel a where

    FreshDone :: NormalizedDisjT rel a -> NormalizedFreshT rel a
    Fresh :: (LogicType x) => 
             FreshType (NormalizedKanrenT rel) x 
          -> (Var' x (NormalizedKanrenT rel) -> NormalizedFreshT rel a) -> NormalizedFreshT rel a

-- Representation
type NormalizedKanrenT rel = NormalizedFreshT rel

-- Instance
instance (Kanren rel) => Kanren (NormalizedKanrenT rel) where
    newtype instance (KVar (NormalizedKanrenT rel)) a = NV (KVar rel a)

    fresh_ = Fresh
    unify = flatteningUnify (\x t -> FreshDone $ disj [raise (conj [Unify x t]) ()])

    call_ (Relation s r) = FreshDone $ disj [raise (conj [Call s r]) ()]

-- Evaluation function
normalize :: (Kanren rel) => NormalizedKanrenT rel a -> rel a
normalize = ...
