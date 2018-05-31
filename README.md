# Haskell treap implementation
A simple [treap](https://en.wikipedia.org/wiki/Treap) written in Haskell, for the [non-procedural programming course](https://is.cuni.cz/studium/predmety/index.php?do=predmet&kod=NPRG005&skr=2017&fak=11320) at Charles University.

## TODO list
```Haskell
split :: Ord a => a -> Set a -> (Set a, Set a) 
lookupIndex :: Ord a => a -> Set a -> Maybe Int
findIndex :: Ord a => a -> Set a -> Int
elemAt :: Int -> Set a -> a 
```
- zjistit, co je na k-tém místě (případně to odstranit)
- zjistit, na kolikátém místě je prvek s danou hodnotou
- operace split a merge - rozdělení na dvě treapy podle nějaké hodnoty a sloučení
- testy korektnosti a rychlosti
