data Field a var where
  Field :: LogicType x => Logic x var -> Field a var

data Constructor a = Constructor
  { name :: String
  , construct :: forall var. [Field a var] -> WithLogic a var
  }

data Logic a (var :: Type -> Type) = Free (Var a var) | Ground (WithLogic a var)
type Var x (var :: Type -> Type) = var (Logic x var)

class LogicType a where
  data WithLogic a (var :: Type -> Type) :: Type
  project :: a -> WithLogic a var
  reify :: WithLogic a var -> Maybe a
  unifyVal :: (Alternative rel) => 
              (forall t. LogicType t => Logic t var -> Logic t var -> rel ()) 
              -> WithLogic a var -> WithLogic a var -> rel ()
  derefVal :: (Alternative rel) => 
              (forall t. LogicType t => Logic t var -> rel t) 
              -> WithLogic a var -> rel a
  generate :: Stream a
  
  quote :: WithLogic a var -> (Constructor a, [Field a var])
  uncurry construct . quote == id


reify . project == Just
project . fromJust . reify == id -- if fromJust is defined 
