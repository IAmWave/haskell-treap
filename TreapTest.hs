module TestLambda where

import qualified Data.List as List
import System.Random
import Test.HUnit

import Treap


ex = Treap 2 9 'h' Empty (Treap 1 7 'j' Empty Empty)
eg = mkStdGen 123

(g1, t1) = insert eg 'd' empty
(g2, t2) = insert g1 'a' t1
t = snd $ fromList eg [1..10]

-- Inefficient but good enough for our purposes
shuffle :: (RandomGen g, Ord a) => g -> [a] -> (g, [a])
shuffle g [] = (g, [])
shuffle g l = (g2, lm:rest)
    where
        (choice,g1) = randomR (0,(length l)-1) g
        ll = take choice l
        lr' = drop choice l
        lm = head lr'
        lr = tail lr'
        (g2,rest) = shuffle g1 (ll++lr)

-- Check whether keys form a tree and weights form a heap
isValid :: Ord a => Treap a -> Bool
isValid t = ((List.sort tl) == tl) && (isValid' t (maxBound))
    where
        tl = toList t
        isValid' :: Treap a -> Weight -> Bool
        isValid' Empty _ = True
        isValid' (Treap _ w _ ls rs) maxW = (w <= maxW) && (isValid' ls w) && (isValid' rs w)

construct :: (RandomGen g, Ord a) => g -> [a] -> (g, [Treap a])
construct g al = (fst $ last scanned, map snd scanned)
    where
        scanned = scanl (\(g, t) a -> insert g a t) (g,Empty) al

testSize :: Test
testSize = TestList $ map makeTest (zip treapList [0..(length treapList)])
    where
        makeTest (t,sz) = TestCase $ assertEqual "Treap size is correct" (size t) sz
        treapList = snd $ construct eg $ shuffledList
        shuffledList = snd $ shuffle eg [1..100]


main :: IO Counts
main = runTestTT $ TestList [testSize]
