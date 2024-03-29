{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Adjunction between a product and a function object
-- https://bartoszmilewski.com/2016/04/18/adjunctions/

main = putStrLn "\nSuccess!\n"

data Pair a b = P a b

instance Functor (Pair a) where
  fmap f (P a b) = P a (f b)

class (Functor f) => Representable f where
  type Rep f :: *
  tabulate :: (Rep f -> x) -> f x
  index    :: f x -> Rep f -> x

data FunctionObj a b = FnObj (a -> b)
-- there exists eval :: (FunctionObj a b, a) -> b
-- for any other z with g :: (z, a) -> b
-- exists unique h :: z -> FunctionObj a b
-- such that g = eval . (h, id)

instance Functor (FunctionObj a) where
  fmap f (FnObj g) = FnObj (f.g)

instance Representable (FunctionObj a) where
  type Rep (FunctionObj a) = a
  tabulate = FnObj
  index (FnObj g) = g

class (Functor f, Representable u) =>
      Adjunction f u | f -> u, u -> f where
  unit         :: a -> u (f a)
  counit       :: f (u a) -> a
  leftAdjunct  :: (f a -> b) -> (a -> u b)
  rightAdjunct :: (a -> u b) -> (f a -> b)

  unit           = leftAdjunct id
  counit         = rightAdjunct id
  leftAdjunct f  = fmap f . unit
  rightAdjunct f = counit . fmap f

instance Adjunction (Pair a) (FunctionObj a) where
  unit b = tabulate (\a -> P a b)
  counit (P a (FnObj f)) = index (FnObj f) a
  rightAdjunct f (P a b) = index (f b) a
  leftAdjunct f b = FnObj (f . \a -> P a b)
