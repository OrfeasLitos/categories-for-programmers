{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Set as Set

-- Adjunction between a product and a function object
-- https://bartoszmilewski.com/2016/04/18/adjunctions/

-- Product with fixed a
data L a z = L a z

instance Functor (L a) where
  fmap f (L a z) = L a (f z)

main = putStrLn "Hello World!"

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

data FunctionObj f a b = FunctionObj f a b
eval :: (FunctionObj (a -> b) a b) -> a -> b
eval (FunctionObj f _ _) x = f x

instance Representable (FunctionObj (a -> b) a) where
  -- https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Set.html
  type Rep (FunctionObj (a -> b) a) = Set.Set a
  tabulate g = Cons (f 0) (tabulate (f . (+1)))
  index (FunctionObj g a b) x = g x

----class (Functor f, Representable u) =>
----      Adjunction f u | f -> u, u -> f where
----  unit         :: a -> u (f a)
----  counit       :: f (u a) -> a
----  leftAdjunct  :: (f a -> b) -> (a -> u b)
----  rightAdjunct :: (a -> u b) -> (f a -> b)
----
----  unit           = leftAdjunct id
----  counit         = rightAdjunct id
----  leftAdjunct f  = fmap f . unit
----  rightAdjunct f = counit . fmap f
----
----
----instance Adjunction Prod FunctionObj where
