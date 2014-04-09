-- наивный 

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- хвост рекурсивный 

fib2 :: Integer -> Integer
fib2 0 = 0
fib2 x = fib2tail 0 1 1
  where
    fib2tail secondPrev prev current
	  | current == x = prev
	  | otherwise = fib2tail prev (secondPrev + prev) (current + 1)




-- с использованием мемоизации
fibmemo n = fibmemolist !! n                                                    
  where
    fibmemolist = 0 : 1 : zipWith (+) fibmemolist (tail fibmemolist)

{-
<0> : 1 : ?
<1> : ?

0 + 1 ==> 1

0 : <1> : 1 : ?
1 : <1> : ?

1 + 1 ==> 2

0 : 1 : <1> : 2 : ?
1 : 1 : <2> : ?

1 + 2 ==> 3

0 : 1 : 1 : <2> : 3 : ?
1 : 1 : 2 : <3> : ?
-}
