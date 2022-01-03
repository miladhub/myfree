module Free where

data MyFree f r = MyFree (f (MyFree f r)) | MyPure r

instance (Functor f) => Functor (MyFree f) where
  fmap f (MyPure a) = MyPure (f a)
  fmap f (MyFree x) = MyFree $ fmap (fmap f) x

instance (Functor f) => Applicative (MyFree f) where
  pure = MyPure
  MyPure a <*> MyPure b  = MyPure (a b)
  MyPure a <*> MyFree fb = MyFree $ fmap (fmap a) fb
  MyFree fa <*> b      = MyFree $ fmap (<*> b) fa

instance (Functor f) => Monad (MyFree f) where
  return = pure
  -- x :: f (MyFree f a)
  -- f :: a -> MyFree f b
  -- fmap (>>= f) x :: f (MyFree f b)
  (MyFree x) >>= f = MyFree $ fmap (>>= f) x
  (MyPure r) >>= f = f r

data Toy b next =
    Output b next
  | Bell next
  | Done

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

output :: a -> MyFree (Toy a) ()
output x = MyFree (Output x (MyPure ()))

