module Main where

import           Control.Monad.Free

main :: IO ()
main = putStrLn "foo"

data Toy b next =
    Output b next
  | Bell next
  | Done
  deriving (Show, Eq)

instance Functor (Toy b) where
    fmap f (Output x next) = Output x (f next)
    fmap f (Bell     next) = Bell     (f next)
    fmap f  Done           = Done

type FToy a = Free (Toy a) ()

output :: a -> FToy a
output x = Free (Output x (Pure ()))

bell :: FToy a
bell = Free (Bell (Pure ()))

done :: FToy a
done = Free Done

program :: FToy Char
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
  fmap f Baz         = Baz

showFoo :: (Show a, Show n) => Free (Foo a) n -> String
showFoo (Pure a)         = "return " ++ (show a) ++ "\n"
showFoo (Free Baz)       = "Baz\n"
showFoo (Free (Bar a n)) = "Bar " ++ (show a) ++ "\n" ++ (showFoo n)

program' :: Free (Foo String) Int
program' = do
    Pure 42
    Free (Bar "foo" (Pure ()))
    Free (Bar "bar" (Pure ()))
    Pure 45
--    Free Baz
    --Free (Bar (Pure 44))
    -- Baz terminates, Pure probably doesn't
{-
 - putStr $ showFoo $ do { Free (Bar 42 (Pure ())); Free (Bar 43 (Pure ())); Pure 45 }
Bar 42
Bar 43
return 45
*Main Lib> putStr $ showFoo $ do { Free (Bar 42 (Pure ())); Free (Bar 43 (Pure ())); Pure 45 }
-}

prod :: String -> Free ((,) String) ()
prod s = Free ((,) s (Pure()))

prog :: Free ((,) String) ()
prog = do
  prod "foo"
  prod "bar"

dump :: Free ((,) String) () -> String
dump (Free ((,) s next)) = s ++ " " ++ (dump next)
dump (Pure ()) = ""

{-
λ> fmap (+1) $ prod "foo" 2
("foo",3)
λ> Free $ prod "foo" (Pure())
Free ("foo",Pure ())
λ> Free $ prod "foo" (Pure()) >>= Free $ prod "bar" (Pure())
λ> Free $ prod "foo" (Pure())
Free ("foo",Pure ())
λ> :t Free $ prod "foo" (Pure())
Free $ prod "foo" (Pure()) :: Free ((,) [Char]) ()
λ> (Free $ prod "foo" (Pure())) >>= \_ -> Pure()
Free ("foo",Pure ())
λ> Free ( prod "foo" (Pure()) ) >>= \_ -> Free ( prod "bar" (Pure()) )
Free ("foo",Free ("bar",Pure ()))
λ> pr s = Free (prod s (Pure()))
λ> pr "foo"
Free ("foo",Pure ())
λ> pr "foo" >>= \_ -> pr "bar"
Free ("foo",Free ("bar",Pure ()))
λ> do {  pr "foo"; pr "bar" }
Free ("foo",Free ("bar",Pure ()))
λ> :{
*Main Lib| p = do
*Main Lib|   pr "foo"
*Main Lib|   pr "bar"
*Main Lib| :}
λ> p
Free ("foo",Free ("bar",Pure ()))
-}

{-
data Cmd a next =
    Print String next
  | GetLine (String -> next)
  | Return a

instance Functor (Cmd a) where
  fmap f (Print s next)  = Print s (f next)
  fmap f (GetLine fnext) = GetLine (f . fnext)
  fmap f (Return a)      = Return a

type FCmd a = Free (Cmd ()) a

print' :: String -> FCmd ()
print' s = Free (Print s (Pure ())) 

read' :: FCmd String
read' = Free (GetLine Pure)

return' :: a -> FCmd a
return' = Pure

pr' :: FCmd Int
pr' = do
  r <- read'
  print' "foo"
  return' $ length r

int' :: FCmd a -> IO a
int' (Free (Print s n)) = do
  putStrLn s
  int' n
int' (Free (GetLine f)) = do
  l <- getLine
  int' $ f l
int' (Pure a) = return a
-}

data Cmd next =
    Print String next
  | GetLine (String -> next)

instance Functor Cmd where
  fmap f (Print s next)  = Print s (f next)
  fmap f (GetLine fnext) = GetLine (f . fnext)

type FCmd a = Free Cmd a

print' :: String -> FCmd ()
--print' s = Free (Print s (Pure ())) 
print' s = liftF (Print s ())

read' :: FCmd String
--read' = Free (GetLine Pure)
read' = liftF (GetLine id)
--Free (fmap Pure (GetLine id)) = Free (GetLine (Pure . id)) = Free (GetLine Pure)


return' :: a -> FCmd a
return' = Pure

pr' :: FCmd Int
pr' = do
  r <- read'
  print' "foo"
  return' $ length r

int' :: FCmd a -> IO a
int' (Free (Print s n)) = do     
  putStrLn s
  int' n
int' (Free (GetLine f)) = do
  l <- getLine
  int' $ f l
int' (Pure a) = return a

{-
Free (GetLine Pure) >>= \s -> Free (Print s (Pure ())) 
  = Free (fmap (>>= (\s -> Free (Print s (Pure ())) )) (GetLine Pure))
  = Free (GetLine (>>= (\s -> Free (Print s (Pure ())) )) . Pure)
  = Free (GetLine (\s -> Free (Print s (Pure ()))))
Free a >>= f = Free (fmap (>>= f) a)

---

g = (\s -> Free (Print s (Pure ())) )

    Free (GetLine Pure) >>= g
  = Free (fmap (>>= g) (GetLine Pure))
  = Free (GetLine (>>= g) . Pure)
  = Free (GetLine \s -> (Pure s) >>= g )
  = Free (GetLine \s -> g s )
  = Free (GetLine g)

Pure a >>= f = f a
Free a >>= f = Free (fmap (>>= f) a)
-}
