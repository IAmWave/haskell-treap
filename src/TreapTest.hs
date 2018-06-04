import qualified Data.List as List
import System.Random
import Test.HUnit

import Treap

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
        isValid' (Treap n w _ ls rs) maxW = (w <= maxW) && (isValid' ls w) && (isValid' rs w) &&
                                            (n == (size ls) + (size rs) + 1)

-- Like fromList, but returns the whole list of treaps being constructed step-by-step
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

testValid :: Test
testValid = TestList $ map makeTest treapList
    where
        makeTest t = TestCase $ assertEqual "Treap satisfies invariant" (isValid t) True
        treapList = snd $ construct eg $ shuffledList
        shuffledList = snd $ shuffle eg [1..100]

testDelete :: Test
testDelete = TestList $ concat $ [map sizeTest [1..to], map memberTest [1..to], map validTest [1..to]]
    where
        to = 100
        sizeTest a = TestCase $ assertEqual "Correct size" (size $ delete a treap) ((size treap) - 1)
        memberTest a = TestCase $ assertBool "Not a member" (not (member a $ delete a treap))
        validTest a = TestCase $ assertBool ("Preserves invariant " ++ (show a)) (isValid $ delete a treap)
        treap = snd $ fromList eg [1..to]

testDeleteNonexistent :: Test
testDeleteNonexistent = TestList $ map makeTest [1..100]
    where
        makeTest a = TestCase $ assertEqual
            "Deleting nonexistent is ok"
            (size $ delete a treap)
            (size treap)
        treap = snd $ fromList eg [101..200]

testElemAt :: Test
testElemAt = TestList $ map makeTest [0..99]
    where
        makeTest i = TestCase $ assertEqual "elemAt works" (elemAt i treap) (i+100)
        treap = snd $ fromList eg [100..199]

testFindIndex :: Test
testFindIndex = TestList $ map makeTest [100..199]
    where
        makeTest a = TestCase $ assertEqual "findIndex works" (findIndex a treap) (a-100)
        treap = snd $ fromList eg [100..199]

main :: IO Counts
main = runTestTT $ TestList [testSize, testValid, testDeleteNonexistent, testDelete, testElemAt,
                             testFindIndex]



ex = Treap 2 9 'h' Empty (Treap 1 7 'j' Empty Empty)
eg = mkStdGen 123

(g1, t1) = insert eg 'd' empty
(g2, t2) = insert g1 'a' t1
a0 = snd $ fromList eg [1..10]
a = delete 6 a0
b = snd $ fromList eg [1..20]
