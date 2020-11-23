module Main where

import Lib
import Control.Monad.Free

main :: IO ()
main = someFunc

data Toy b next =
    Output b next
  | Bell next
  | Done
  deriving (Show, Eq)

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

output :: a -> Free (Toy a) ()
output x = Free (Output x (Pure ()))

bell :: Free (Toy a) ()
bell = Free (Bell (Pure ()))

done :: Free (Toy a) r
done = Free Done

program :: Free (Toy Char) r
program = do
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
    "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
    "bell\n" ++ showProgram x
showProgram (Free Done) =
    "done\n"
showProgram (Pure r) =
    "return " ++ show r ++ "\n"

data Foo a n = Bar a n | Baz deriving (Show, Eq)

instance Functor (Foo a) where
  fmap f (Bar a foo) = Bar a (f foo)
  fmap f Baz = Baz

showFoo :: (Show a, Show n) => Free (Foo a) n -> String
showFoo (Pure a)         = "return " ++ (show a) ++ "\n"
showFoo (Free Baz)       = "Baz\n"
showFoo (Free (Bar a n)) = "Bar " ++ (show a) ++ "\n" ++ (showFoo n)

program' :: Free (Foo Char) Int
program' = do
    Pure 42
    Free Baz
    --Free (Bar (Pure 44))
    --
{-
 - putStr $ showFoo $ do { Free (Bar 42 (Pure ())); Free (Bar 43 (Pure ())); Pure 45 }
Bar 42
Bar 43
return 45
*Main Lib> putStr $ showFoo $ do { Free (Bar 42 (Pure ())); Free (Bar 43 (Pure ())); Pure 45 }
-}
