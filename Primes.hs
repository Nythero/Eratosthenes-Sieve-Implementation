module Primes(
  primes)
  where 

import PriorityQueue

--Defines a pair where the first element is a multiple of the second element which is a prime number.
data NumberAndFactors = NAF Int Int deriving Show

--Defines that two pairs are equal if the multiple of each are equal.
instance Eq NumberAndFactors where
  (==) (NAF x xs) (NAF y ys) = x == y

--Defines < and > for the type
instance Ord NumberAndFactors where
  compare (NAF x xs) (NAF y ys) = compare x y
  (<=) (NAF x xs) (NAF y ys) = x <= y
  (<) (NAF x xs) (NAF y ys) = x < y
  (>) (NAF x xs) (NAF y ys) = x > y
  (>=) (NAF x xs) (NAF y ys) = x >= y

--Returns an infinite list of prime numbers
primes :: [Int]
primes = 2 : sieve [3..] (insertPQ (NAF 4 2) emptyPQ)

--Filters the elements of the list
sieve :: [Int] -> PriorityQueue NumberAndFactors -> [Int]
sieve [] pq = []
sieve (x:xs) pq = let minNumber = number (findMinPQ pq) 
  in if minNumber == x
    then sieve xs (insertPQ (incrementByFactor(findMinPQ pq)) (deleteMinPQ pq))
    else if minNumber > x
      --If the lowest element of the PriorityQueue is greater than x then x is prime
      then x : sieve xs (insertPQ (NAF (x*x) x) pq)
      --If x is greater then removes the lowest element from the Priority Queue and test the number again since 
      --multiple primes can divide the same number but in the equal condition only removes one ocurrence from the
      --Priority Queue.
      else sieve (x:xs) (insertPQ (incrementByFactor(findMinPQ pq)) (deleteMinPQ pq))

incrementByFactor :: NumberAndFactors -> NumberAndFactors
incrementByFactor (NAF x y) = NAF (x+y) y

number :: NumberAndFactors -> Int
number (NAF x y) = x
