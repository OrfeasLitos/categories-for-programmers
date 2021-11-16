{-# LANGUAGE TypeFamilies #-}

-- Input
--------

class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index    :: f x -> Rep f -> x

data Pair a = Pair a a

-- Solution
-----------

instance Representable Pair where
    type Rep Pair = Bool
    tabulate f = Pair (f True) (f False)
    index (Pair x y) b = if b then x else y


-- Tests
--------

instance Show a => Show (Pair a) where
    show (Pair x y) = "Pair " ++ show x ++ " " ++ show y

f True = 1
f False = 0

tabbedF :: Pair Int
tabbedF = tabulate f

g :: (Rep Pair) -> String
g True = "True"
g False = "False"

tabbedG :: Pair String
tabbedG = tabulate g

main = do
    putStr $ show $ tabbedF
    putStrLn $ " should be a pair of " ++ (show $ f True) ++ " " ++ (show $ f False)

    putStr $ show $ tabbedG
    putStrLn $ " should be a pair of " ++ (show $ g True) ++ " " ++ (show $ g False)

    putStr $ index (Pair "a" "b") True
    putStrLn $ " should be a"

    putStr $ index (Pair "a" "b") False
    putStrLn $ " should be b"
