class (Kanren (UnderlyingRel rel), Applicative (NRaise rel), Alternative (NDisj rel)) => 
      NormalizedKanren (rel :: Type -> Type) where

    type UnderlyingRel rel :: Type -> Type

    data NBase rel :: Type
    data NConj rel :: Type
    data NRaise rel :: Type -> Type
    data NDisj rel :: Type -> Type

    unifyVarNorm :: (LogicType a) => 
                    Var' a (UnderlyingRel rel) -> L a (UnderlyingRel rel) -> NBase rel

    callNorm_ :: (String, rel) -> NBase rel

    freshNorm_ :: (LogicType a) => 
                  FreshType (UnderlyingRel rel) a 
               -> (Var' a (UnderlyingRel rel) -> rel x) -> rel x

    liftBase :: NBase rel -> NConj rel
    liftConj :: NConj rel -> a -> NRaise rel a
    liftRaise :: NRaise rel a -> NDisj rel a
    liftDisj :: NDisj rel a -> rel a
    
    makeConj :: [NBase rel] -> NConj rel
    makeDisj :: [NRaise rel a] -> NDisj rel a

interpretNorm :: (NormalizedKanren nrel) => NormalizedKanrenT (UnderlyingRel nrel) a -> nrel a
interpretNorm = ...
