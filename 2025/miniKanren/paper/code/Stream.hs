data Stream a = Empty
              | Mature a (Stream a)
              | Immature (Stream a)
              deriving Show

instance Alternative Stream where
  empty = Empty
  (Mature h tl) <|> y = Mature h $ y <|> tl
  (Immature  x) <|> y = Immature $ y <|> x
  Empty         <|> y = y

instance Monad Stream where
  return a = Mature a Empty
  Empty >>= _ = mzero
  Mature x xs >>= g = g x <|> (xs >>= g)
  Immature x  >>= y = Immature $ x >>= y
