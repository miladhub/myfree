module Free where

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free $ fmap (fmap f) x

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b  = Pure (a b)
  Pure a <*> Free fb = Free $ fmap (fmap a) fb
  Free fa <*> b      = Free $ fmap (<*> b) fa

instance (Functor f) => Monad (Free f) where
  return = pure
  (Free x) >>= f = Free $ fmap (>>= f) x
  (Pure r) >>= f = f r

data Toy b next =
    Output b next
  | Bell next
  | Done

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

output :: a -> Free (Toy a) ()
output x = Free (Output x (Pure ()))

