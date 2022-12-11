{-# LANGUAGE TypeFamilies #-}

-- Adjunction between a product and a function object
-- https://bartoszmilewski.com/2016/04/18/adjunctions/

-- Product with fixed a
data Pair a z = P a z

instance Functor (Pair a) where
  fmap f (P a z) = P a (f z)

main = putStrLn "Hello World!"

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

data FunctionObj a b = FunctionObj a b
-- there exists eval :: (FunctionObj b a, a) -> b
-- for any other z with g :: (z, a) -> b
-- exists unique h :: z -> FunctionObj b a
-- such that g = eval . (h, id)

instance Representable (FunctionObj a) where
  type Rep (FunctionObj a) = a
  tabulate g = FunctionObj a (g a)
  index (FunctionObj a b) = g where g a = b

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
