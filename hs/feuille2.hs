-------------------------------------------------------------------------------
-- TITLE: TP Prog Foncitonnelle: Feuille 2
-------------------------------------------------------------------------------
-- imports
import Data.List
import Data.Maybe

-------------------------------------------------------------------------------
-- Exercice 1:
-------------------------------------------------------------------------------
-- shemas:
qqs :: Eq a => (a -> Bool) -> [a] -> Bool
qqs _ [] = True
qqs p (x:xs) = p x && qqs p xs

ies :: Eq a => (a -> Bool) -> [a] -> Bool
ies _ [] = False
ies p (x:xs) = p x || ies p xs

-- Tous egaux:
tousEgauxUni :: Eq a => [a] -> Bool
tousEgauxUni [] = False
tousEgauxUni (x:xs) = qqs (\y -> y == x) xs

tousEgauxEx :: Eq a => [a] -> Bool
tousEgauxEx [] = False
tousEgauxEx (x:xs) = not $ ies (\y -> y /= x) xs

-- Tous diff:
tousDiffUni :: Eq a => [a] -> Bool
tousDiffUni [] = False
tousDiffUni (x:[]) = True
tousDiffUni (x:xs) = qqs (\y -> y /= x) xs && tousDiffEx xs

tousDiffEx :: Eq a => [a] -> Bool
tousDiffEx [] = False
tousDiffEx (x:xs) = not $ ies (\y -> y == x) xs || tousDiffEx xs

-------------------------------------------------------------------------------
-- Exercice 2:
-------------------------------------------------------------------------------
-- shema simple:
sh :: Int -> a -> (Int -> Bool) -> (a -> Int -> a) -> a
sh n init arret f =
  if arret n
    then init
    else f us n
  where
    us = sh (n - 1) init arret f

snInt :: Int -> Int
snInt n = sh n 1 (\x -> x == 1) (+)

snCarreInt :: Int -> Int
snCarreInt n = sh n 1 (== 1) (\x y -> x + y * y)

fact1 :: Int -> Int
fact1 n = sh n 1 (== 1) (*)

fib1 :: Int -> Int
fib1 n = h $ sh n (1, 1) (<= 1) (\(x, y) z -> (x + y, x))
  where
    h (u, v) = u

-- shema double:
sh2 :: Int -> a -> a -> Int -> Int -> (a -> a -> Int -> a) -> a
sh2 n init1 init2 n0 n1 f
  | n == n0 = init1
  | n == n1 = init2
  | otherwise = f us uss n
  where
    us = sh2 (n - 1) init1 init2 n0 n1 f
    uss = sh2 (n - 2) init1 init2 n0 n1 f

fib2 :: Int -> Int
fib2 n = sh2 n 1 1 0 1 (\x y z -> x + y)

-------------------------------------------------------------------------------
-- Exercice 3:
-------------------------------------------------------------------------------
reflexive :: Eq a => (a -> a -> Bool) -> [a] -> Bool
reflexive r e = qqs (\x -> r x x) e

symetrique :: Eq a => (a -> a -> Bool) -> [a] -> Bool
symetrique r e = qqs (\x -> qqs (\y -> aux x y) e) e
  where
    aux x y =
      if r x y
        then r y x
        else True

transitive :: Eq a => (a -> a -> Bool) -> [a] -> Bool
transitive r e = qqs (\x -> qqs (\y -> qqs (\z -> aux x y z) e) e) e
  where
    aux x y z =
      if r x y && r y z
        then r x z
        else True

quotient :: Eq a => (a -> a -> Bool) -> [a] -> [[a]]
quotient _ [] = [[]]
quotient _ (x:[]) = [[x]]
quotient r (x:xs) = add x rs
  where
    rs = quotient r xs
    add v [] = [[v]]
    add v (t:ts) =
      if r x (head t)
        then (x : t) : ts
        else t : add v ts

testEx31 :: [[Int]]
testEx31 = quotient (\x y -> (x - y) `mod` 3 == 0) [0, 1, 2, 3, 4, 5, 6]

-------------------------------------------------------------------------------
-- Exercice 4:
-------------------------------------------------------------------------------
prodCart :: [a] -> Int -> [[a]]
prodCart e 0 = [[]]
prodCart e n = foldr (\x y -> map (\z -> x : z) us ++ y) [] e
  where
    us = prodCart e (n - 1)

-- prodCart [0,1] 3
-------------------------------------------------------------------------------
-- Exercice 5:
-------------------------------------------------------------------------------
chemin :: Int -> Int -> [(Int, [Int])] -> [[Int]]
chemin d a g
  | d == a = [[a]]
  | isJust neibors =
    foldr (\x acc -> (map (d :) $ chemin x a gs) ++ acc) [] $
    snd $ fromJust neibors
  | otherwise = []
  where
    neibors = find (\(s, v) -> s == d) g
    gs = filter (\(s, v) -> s /= d) g
-- chemin 4 2 [(4, [1,3]), (1, [4,3,2]), (3, [2]), (2, [])]
