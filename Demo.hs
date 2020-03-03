module Demo where
import Data.Char

infixl 6 *+*
(*+*) a b = a ^ 2 + b ^ 2
x |-| y = if x >= y then x-y else y-x

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then 10*digitToInt(x)+digitToInt(y) else 100

doubleFact :: Integer -> Integer
doubleFact n = if n == 1 then 1 else if n==2 then 2 else n*doubleFact (n-2)
doubleFact' :: Integer -> Integer
doubleFact' 0 = 1
doubleFact' 1 = 1
doubleFact' n = n * doubleFact (n-2)

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci (-2) = (-1)
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' (-1) = 1
fibonacci' (-2) = (-1)
fibonacci' n | n > 0 = helper 0 1 2 n
	    | n < 0 = helper 1 (-1) (-3) n
helper mem1 mem2 cur n | n > 0 && n==cur = mem1+mem2
                       | n > 0 && n > cur = helper mem2 (mem1+mem2) (cur+1) n
		       | n < 0 && n == cur = mem1-mem2
		       | n < 0 && n < cur = helper mem2 (mem1-mem2) (cur-1) n
                       | otherwise = undefined

--GHCi> sum'n'count (-39)
--(12,2)
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = (sumDigits (abs x), numDigits (abs x))
  where sumDigits x | x < 10 = x
                    | otherwise = mod x 10 + sumDigits (div x 10) 
        numDigits x | x < 10 = 1
                    | otherwise = 1 + numDigits (div x 10)   

--GHCi> integration sin pi 0
---2.0
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = trapeze f a b 1000 where
  trapeze f a b n = step a b n *((f(a) + f(b))/2 + sumf 0 a 1 n) where
    sumf acc a i n | i==n = acc
                   | otherwise = sumf (acc + f(a+i*step a b n)) a (i+1) n
    step a b n = (b-a)/n

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (a:as) | odd a = a:oddsOnly as
                | otherwise = oddsOnly as
