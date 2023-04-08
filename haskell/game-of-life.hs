-- Game of Life using Store Comonad

import Control.Comonad

main = putStrLn "\nSuccess!\n"

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store g s) = Store (f.g) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

-- Rules:
-- 1. Live cells with 2 or 3 live neighbors survive
-- 2. Dead cells with 3 live neighbors go live
-- 3. The rest go/stay dead

-- I think that s :: Int -> Int -> Nat
-- Alternatively s :: Nat

data Stream a = Cons a (Stream a)
data Row = Row (Stream Bool)
data Grid = Grid (Stream Row)
data Game = Game (Stream Grid)

data GoL = Store (
