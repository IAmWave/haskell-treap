import System.Random
import qualified Data.List as List

type Weight = Int

-- Elements with higher weights are higher.
data Treap v = Empty | Treap Weight v (Treap v) (Treap v)
    deriving (Eq)

instance Show a => Show (Treap a) where
    show = showTree
    -- show Empty = ""
    -- show (Treap w a ls rs) = (show a)++"|"++(show w)++"(" ++ (show ls) ++ ";" ++ (show rs) ++ ")"


ex = Treap 9 'h' Empty (Treap 7 'j' Empty Empty)
eg = mkStdGen 123

empty :: Treap a
empty = Empty

singleton :: RandomGen g => g -> a -> (g, Treap a)
singleton g a = (g', Treap w a Empty Empty)
    where (w, g') = next g

-------------- Lookup --------------
isEmpty :: Treap a -> Bool
isEmpty Empty = True
isEmpty t = False

member :: Ord a => a -> Treap a -> Bool
member _ Empty = False
member a (Treap _ v ls rs)
    | a < v     = member a ls
    | a == v    = True
    | a > v     = member a rs


-------------- Modification --------------
insert :: (RandomGen g, Ord a) => g -> a -> Treap a -> (g, Treap a)
insert g a treap = (g', insertWithWeight w a treap)
    where (w, g') = next g

-- First insert at the bottom, then rotate to maintain the heap invariant
insertWithWeight :: Ord a => Weight -> a -> Treap a -> Treap a
insertWithWeight w a Empty = Treap w a Empty Empty
insertWithWeight w a (Treap w' a' ls' rs')
    | a <= a' = rotate $ Treap w' a' (insertWithWeight w a ls') rs'
    | a >  a' = rotate $ Treap w' a' ls' (insertWithWeight w a rs')

rotate :: Ord a => Treap a -> Treap a
rotate t = rotateL (rotateR t)

-- Maintain the heap invariant of the left son
rotateL :: Ord a => Treap a -> Treap a
rotateL Empty = Empty
rotateL (Treap w a Empty rs) = Treap w a Empty rs
rotateL (Treap w a (Treap lw la lls lrs) rs)
    | lw > w      = Treap lw la lls (Treap w a lrs rs)
    | otherwise   = (Treap w a (Treap lw la lls lrs) rs)

-- Maintain the heap invariant of the right son
rotateR :: Ord a => Treap a -> Treap a
rotateR Empty = Empty
rotateR (Treap w a ls Empty) = Treap w a ls Empty
rotateR (Treap w a ls (Treap rw ra rls rrs))
    | rw > w      = Treap rw ra (Treap w a ls rls) rrs
    | otherwise   = Treap w a ls (Treap rw ra rls rrs)

-------------- List conversion --------------
fromList :: (RandomGen g, Ord a) => g -> [a] -> (g, Treap a)
fromList g [] = (g, Empty)
fromList g (a:as) = insert g' a rest
    where (g', rest) = fromList g as

toList :: Treap a -> [a]
toList l = toList' l []
    where
        toList' Empty acc = acc
        toList' (Treap _ a ls rs) acc0 = acc2
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
showTree' (Treap w a ls rs) d = myLine ++ "\n" ++ rest
    where
        myLine = (indent d) ++ (show a) ++ " (" ++ (show w) ++ ")"
        rest = if ((isEmpty ls) && (isEmpty rs))
            then ""
            else ((showTree' ls (True:d)) ++ (showTree' rs (False:d)))

(g1, t1) = insert eg 'd' empty
(g2, t2) = insert g1 'a' t1
t = snd $ fromList eg [1..10]
