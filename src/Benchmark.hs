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

main = defaultMain [
    -- performance of `fromList` gives us the performance of `insert` (with minial overhead)
    bgroup "fromList" [
        bench "1e3" $ whnf (snd . (fromList rng) . range) 1000,
        bench "1e4" $ whnf (snd . (fromList rng) . range) 10000,
        bench "1e5" $ whnf (snd . (fromList rng) . range) 100000,
        -- Does the insertion order matter?
        bench "1e3m" $ whnf (snd . (fromList rng) . middleRange) 1000,
        bench "1e4m" $ whnf (snd . (fromList rng) . middleRange) 10000,
        bench "1e5m" $ whnf (snd . (fromList rng) . middleRange) 100000
        ]
    ]
