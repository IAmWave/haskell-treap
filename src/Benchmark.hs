import System.Random
import Criterion.Main
import Treap

rng :: StdGen
rng = mkStdGen 188 -- one arbitrary generator, (hopefully) doesn't matter to us

range :: Int -> [Int]
range n = [1..n]

-- middleRange 10 = [1,10,2,9,3,8,4,7,5,6]
middleRange :: Int -> [Int]
middleRange n = concat $ zipWith (\x y -> [x,y]) [1..half] (reverse [(half+1)..n])
    where
        half = n `div` 2

deleteAll :: Ord a => [a] -> Treap a -> Bool
deleteAll l t = Treap.null $ foldl (\t a -> delete a t) t l

main = defaultMain [
        fromListBenchmarks,
        deleteBenchmarks,
        lookupBenchmarks "member" member,
        lookupBenchmarks "elemAt" (\x t -> elemAt (x-1) t),
        lookupBenchmarks "findIndex" findIndex
    ]

-- performance of `fromList` gives us the performance of `insert` (with minial overhead).
-- Different sizes show how fast the operations are asymptotically
fromListBenchmarks = bgroup "insert" [
        bench "1e3" $ whnf fromList' [1..1000],
        bench "1e4" $ whnf fromList' [1..10000],
        bench "2e4" $ whnf fromList' [1..20000],
        -- Does the insertion order matter?
        bench "2e4m1" $ whnf fromList' (middleRange 20000),
        bench "2e4m2" $ whnf fromList' (reverse $ middleRange 20000)
    ]
    where fromList' = snd . (fromList rng)

deleteBenchmarks = bgroup "delete" [
        bench "1e3" $ deleteAllTest [1..1000],
        bench "1e4" $ deleteAllTest [1..10000],
        bench "2e4" $ deleteAllTest [1..20000],
        -- Does the deletion order matter?
        bench "2e4m1" $ deleteAllTest (middleRange 20000),
        bench "2e4m2" $ deleteAllTest (reverse $ middleRange 20000)
    ]
    where deleteAllTest l = whnf (deleteAll l) t
            where
                -- The where clause is evaluated only once meaning that we don't count
                -- the overhead from running fromList' into the time
                t = snd . (fromList rng) $ l

lookupBenchmarks name f = bgroup name [
        bench "2e4" $ memberTest [1..20000],
        bench "2e4m1" $ memberTest (middleRange 20000),
        bench "2e4m2" $ memberTest (reverse $ middleRange 20000)
    ]
    where memberTest l = nf (map (\x -> f x t)) l
            where t = snd . (fromList rng) $ l
