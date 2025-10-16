-- Interface of a substitution
emptySubst :: Subst s
readSubst :: V s a -> Subst s -> Maybe a
updateSubst :: V s a -> Maybe a -> Subst s -> Subst s

nextVar :: Subst s -> Int
succVar :: Subst s -> Subst s

-- Representation
newtype SubstKanren s a = SubstKanren (StateT (Subst s) Stream a) 
                          deriving (Functor, Applicative, Alternative, Monad)

makeVar :: Maybe (L a (SubstKanren s)) -> R s (Var' a (R s))
makeVar x = do
    v <- SVar <$> gets nextVar
    modify $ succVar . updateSubst v x
    return $ v

type V s t = KVar (SubstKanren s) t
instance Kanren (SubstKanren s) where
    newtype instance (KVar (SubstKanren s)) t = SVar Int deriving (Functor, Show)

    fresh_ FreshVar   = (makeVar Nothing >>=)
    fresh_ (ArgVar x) = (makeVar (Just x) >>=)
    unify = flatteningUnify unif
        where
            unif v y | occursCheck v y = empty
                     | otherwise = do
                        x <- gets $ readSubst v
                        case x of
                            Nothing -> modify $ updateSubst v (Just y)
                            Just x' -> unify x' y
    call_ (name, (SubstKanren r)) = SubstKanren $ mapStateT Immature r

instance KanrenEval (SubstKanren s) where
    derefVar v = do
        x <- gets $ readSubst v
        case x of
            Nothing -> SubstKanren $ lift $ Immature $ generate
            Just x' -> eval x'

-- Evaluation function:
runSubstKanren :: (forall s. SubstKanren s t) -> Stream t 
runSubstKanren (SubstKanren r) = evalStateT r emptySubst
