module Primes(
  primes)
  where 

import PriorityQueue

data NumberAndFactors = NAF Int Int deriving Show

instance Eq NumberAndFactors where
  (==) (NAF x xs) (NAF y ys) = x == y

instance Ord NumberAndFactors where
  compare (NAF x xs) (NAF y ys) = compare x y
  (<=) (NAF x xs) (NAF y ys) = x <= y
  (<) (NAF x xs) (NAF y ys) = x < y
  (>) (NAF x xs) (NAF y ys) = x > y
  (>=) (NAF x xs) (NAF y ys) = x >= y

primes :: [Int]
primes = 2 : sieve [3..] (insertPQ (NAF 4 2) emptyPQ)

primes' :: [Int]
primes' = sieve' [2..] 

sieve :: [Int] -> PriorityQueue NumberAndFactors -> [Int]
sieve [] pq = []
sieve (x:xs) pq = let minNumber = number (findMinPQ pq) --O(1)
  in if minNumber == x
    then sieve xs (insertPQ (incrementByFactor(findMinPQ pq)) (deleteMinPQ pq))
    else if minNumber > x
      then x : sieve xs (insertPQ (NAF (x*x) x) pq)
      else sieve (x:xs) (insertPQ (incrementByFactor(findMinPQ pq)) (deleteMinPQ pq))

incrementByFactor :: NumberAndFactors -> NumberAndFactors
incrementByFactor (NAF x y) = NAF (x+y) y

number :: NumberAndFactors -> Int
number (NAF x y) = x

sieve' :: [Int] -> [Int]
sieve' (p:ps) = p : sieve' [x | x <- ps, mod x p /= 0]
