module Main where

-- наивный
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

{-
fact 2
2 * fact 1
2 * (1 * fact 0)
2 * (1 * 1)
2 * 1
2
-}

-- хвост рекурсивный
fact2 x = tailFact x 1
  where tailFact 0 a = a
        tailFact n a = tailFact (n - 1) (n * a)

{-
fact2 2
tailFact 2 1
tailFact (2 - 1) (2 * 1)
tailFact 1 2
tailFact (1 - 1) (1 * 2)
tailFact 0 2
2
-}

-- с использованием мемоизации
facs :: [Int]               -- значение

-- мемоизация с помощью списка
facs = scanl (*) 1 [1..]    -- промежуточные значения свёртки слева(бесконечный список факториалов)

ffac n = facs !! n          -- ищем по индексу какой факториал нам нужен

