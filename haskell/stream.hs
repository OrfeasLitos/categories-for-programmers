{-# LANGUAGE TypeFamilies #-}

import System.CPUTime

-- Input
--------

class Representable f where
    type Rep f :: *
    tabulate :: (Rep f -> x) -> f x
    index    :: f x -> Rep f -> x

data Stream x = Cons x (Stream x)

instance Representable Stream where
    type Rep Stream = Integer
    tabulate f = Cons (f 0) (tabulate (f . (+1)))
    index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

f x = x*x

-- Solutions
------------

-- good solution
memoizedF :: Stream Integer
memoizedF = tabulate f

-- bad solution
memoize :: (Rep Stream -> Integer) -> Stream Integer
memoize g = tabulate g

-- Benchmark
------------

input = 100000

main = do
  start <- getCPUTime

  afterFirstGoodMemoized <- index memoizedF input `seq` getCPUTime
  afterSecondGoodMemoized <- index memoizedF input `seq` getCPUTime

  afterFirstBadMemoized <- index (memoize f) input `seq` getCPUTime
  afterSecondBadMemoized <- index (memoize f) input `seq` getCPUTime

  afterFirstNonmemoized <- f input `seq` getCPUTime
  afterSecondNonmemoized <- f input `seq` getCPUTime

  putStrLn "\nThe second call to good-memoized should be much faster,"
  putStrLn "since the same Stream is reused in the second call:"
  putStrLn $ "First good memoized:  " ++ (show $ afterFirstGoodMemoized - start) ++ " ns"
  putStrLn $ "Second good memoized: " ++ (show $ afterSecondGoodMemoized - afterFirstGoodMemoized) ++ " ns"

  putStrLn "\nThe two calls to bad-memoized should take about the same time,"
  putStrLn "since a new Stream is made from scratch in the second call:"
  putStrLn $ "First bad memoized:   " ++ (show $ afterFirstBadMemoized - afterSecondGoodMemoized) ++ " ns"
  putStrLn $ "Second bad memoized:  " ++ (show $ afterSecondBadMemoized - afterFirstBadMemoized) ++ " ns"

  putStrLn "\nBaseline (it's way faster because there's no linear recursive data structure):"
  putStrLn $ "First non-memoized:   " ++ (show $ afterFirstNonmemoized - afterSecondBadMemoized) ++ " ns"
  putStrLn $ "Second non-memoized:  " ++ (show $ afterSecondNonmemoized - afterFirstNonmemoized) ++ " ns"
