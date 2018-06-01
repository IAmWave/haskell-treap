module Treap where

import System.Random

type Weight = Int
type Size = Int

-- Elements with higher weights are higher.
data Treap v = Empty | Treap Size Weight v (Treap v) (Treap v)
    deriving (Eq)

instance Show a => Show (Treap a) where
    show = showTree
    -- show Empty = ""
    -- show (Treap w a ls rs) = (show a)++"|"++(show w)++"(" ++ (show ls) ++ ";" ++ (show rs) ++ ")"

empty :: Treap a
empty = Empty

singleton :: RandomGen g => g -> a -> (g, Treap a)
singleton g a = (g', Treap 1 w a Empty Empty)
    where (w, g') = next g

-------------- Lookup --------------
isEmpty :: Treap a -> Bool
isEmpty Empty = True
isEmpty t = False

size :: Treap a -> Size
size Empty = 0
size (Treap n _ _ _ _) = n

member :: Ord a => a -> Treap a -> Bool
member _ Empty = False
member a (Treap _ _ v ls rs)
    | a < v     = member a ls
    | a == v    = True
    | a > v     = member a rs


-------------- Modification --------------
insert :: (RandomGen g, Ord a) => g -> a -> Treap a -> (g, Treap a)
insert g a treap = (g', insertWithWeight w a treap)
    where (w, g') = next g

-- First insert at the bottom, then rotate to maintain the heap invariant
insertWithWeight :: Ord a => Weight -> a -> Treap a -> Treap a
insertWithWeight w a Empty = Treap 1 w a Empty Empty
insertWithWeight w a (Treap n w' a' ls' rs')
    | a <= a' = rotate $ Treap (n+1) w' a' (insertWithWeight w a ls') rs'
    | a >  a' = rotate $ Treap (n+1) w' a' ls' (insertWithWeight w a rs')

rotate :: Ord a => Treap a -> Treap a
rotate = rotateL . rotateR

-- Maintain the heap invariant of the left son
rotateL :: Ord a => Treap a -> Treap a
rotateL Empty = Empty
rotateL (Treap n w a Empty rs) = Treap n w a Empty rs
rotateL (Treap n w a (Treap ln lw la lls lrs) rs)
    | lw > w      = updateSize $ Treap ln lw la lls (updateSize $ Treap n w a lrs rs)
    | otherwise   = (Treap n w a (Treap ln lw la lls lrs) rs) -- no rotation

-- Maintain the heap invariant of the right son
rotateR :: Ord a => Treap a -> Treap a
rotateR Empty = Empty
rotateR (Treap n w a ls Empty) = Treap n w a ls Empty
rotateR (Treap n w a ls (Treap rn rw ra rls rrs))
    | rw > w      = updateSize $ Treap rn rw ra (updateSize $ Treap n w a ls rls) rrs
    | otherwise   = Treap n w a ls (Treap rn rw ra rls rrs) -- no rotation

-- Recalculate size from the sons' sizes
updateSize :: Treap a -> Treap a
updateSize Empty = Empty
updateSize (Treap n w v ls rs) = (Treap ((size ls) + (size rs) + 1) w v ls rs)

delete :: Ord a => a -> Treap a -> Treap a
delete a Empty = Empty      -- No error - mirrors behavior of Data.Set
delete a (Treap n' w' a' ls rs)
    | a < a'     = updateSize $ Treap n' w' a' (delete a ls) rs
    | a > a'     = updateSize $ Treap n' w' a' ls (delete a rs)
    | ls == Empty && rs == Empty = Empty
    | ls == Empty   = rs
    | rs == Empty   = ls
    | otherwise     = rotate $ (Treap (n'-1) w2 a2 ls t2) -- Both sons exist
        where (t2, w2, a2) = delete' rs

-- Find the minimum, remove it and return the removed element along with the modified treap
delete' :: Ord a => Treap a -> (Treap a, Weight, a)
delete' (Treap n w a Empty rs) = (rs, w, a)
delete' (Treap n w a ls rs) = (rotate $ Treap (n-1) w a ls' rs, w', a')
    where (ls', w', a') = delete' ls

-------------- List conversion --------------
fromList :: (RandomGen g, Ord a) => g -> [a] -> (g, Treap a)
fromList g [] = (g, Empty)
fromList g (a:as) = insert g' a rest
    where (g', rest) = fromList g as

toList :: Treap a -> [a]
toList l = toList' l []
    where
        toList' Empty acc = acc
        toList' (Treap _ _ a ls rs) acc0 = acc2
            where acc2 = toList' ls (a:acc1)
                  acc1 = toList' rs acc0


-------------- Debugging --------------
showTree :: Show a => Treap a -> String
showTree t = showTree' t []

indent :: [Bool] -> String
indent [] = ""
indent (x:xs) = (foldl (++) "" $ reverse $ map (\x -> if x then "|  " else "   ") xs) ++ "+--"

showTree' :: Show a => Treap a -> [Bool] -> String
showTree' Empty d = (indent d) ++ "ø\n"
showTree' (Treap _ w a ls rs) d = myLine ++ "\n" ++ rest
    where
        myLine = (indent d) ++ (show a) ++ " (" ++ (show w) ++ ")"
        rest = if ((isEmpty ls) && (isEmpty rs))
            then ""
            else ((showTree' ls (True:d)) ++ (showTree' rs (False:d)))
