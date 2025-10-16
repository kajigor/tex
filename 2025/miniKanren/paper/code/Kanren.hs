data FreshType (rel :: Type -> Type) (t :: Type) where
    FreshVar :: FreshType rel t
    ArgVar :: (Kanren rel, LogicType t) => Logic t (KVar rel) -> FreshType rel t


class (Alternative rel, Functor (KVar rel)) => Kanren rel where
    data KVar rel :: Type -> Type
    unify :: (LogicType a) => Logic a (KVar rel) -> Logic a (KVar rel) -> rel ()
    call_ :: (String, rel ()) -> rel ()
    fresh_ :: (LogicType t) => FreshType rel t -> (Var t (KVar rel) -> rel a) -> rel a

class (Kanren rel) => KanrenEval rel where
    derefVar :: (LogicType a) => Var a (KVar rel) -> rel a

eval :: (KanrenEval rel, LogicType a) => Logic a (KVar rel) -> rel a
eval (Free v) = derefVar v
eval (Ground x) = derefVal eval x
