-- StackOverflow question https://stackoverflow.com/questions/74756249/use-declared-variable-in-instance-definition-in-haskell
{-# LANGUAGE TypeFamilies #-}

class Representable f where
   type Rep f :: *
   tabulate :: (Rep f -> x) -> f x
   index    :: f x -> Rep f -> x

data F a b = F a b

instance Representable (F a) where
--                        ^
-- orfeas: It's right here, ghc!
  type Rep (F a) = a
  tabulate g = F a (g a)
  --             ^    ^
  -- ghc:        a not in scope :(
  index (F a b) = g where g a = b

main = return ()
