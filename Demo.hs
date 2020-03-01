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
