import Data.List
assigner :: [String] -> Int -> [(String,Int)]
assigner [] x = []
assigner (x:xs) y 
 | nub x == ['.'] = assigner xs (y+1000000)
 |otherwise = [(x,y)] ++ assigner xs (y+1)


funny :: (String,Int) -> Int -> [(String,Int)]
funny (a,b) y 
 | y==b = [(a,b)] 
 | otherwise = [(a,y)] ++ funny (a,b) (y-1)

reassigner :: (String,Int) -> Int -> [((Int,Int))]
reassigner ([],x) y = []
reassigner ((x:xs),y) z 
 | x == '#' = [(y,z)] ++ reassigner (xs,y) (z+1)
 | otherwise = reassigner (xs,y) (z+1)

coladjust :: [((Int,Int))] -> Int -> [((Int,Int))] ->[((Int,Int))]
coladjust x y z 
 | y >= maximum(map (\k -> snd k) x)  = x
 | (filter (\z -> snd z == y) x) == [] = coladjust (map (\z -> fun z y) x) (y+1000000) z
 | otherwise = coladjust x (y+1) z

fun :: (Int,Int) -> Int -> (Int,Int)
fun z y 
 | snd(z)>=y = ((fst z),(snd(z) + 999999))
 | otherwise = ((fst z), (snd z))

funn :: (Int,Int) -> Int -> (Int,Int)
funn z y 
 | snd(z)>=y = ((fst z),(snd(z) + 2))
 | otherwise = ((fst z), (snd z))

final :: [((Int,Int))] -> Int 
final [] = 0
final (x:xs) = sum (map (\y -> abs(snd(y)-snd(x)) +abs(fst(y)-fst(x))) xs) + final xs 

main :: IO ()
main = do

    input <- readFile "Day-11.txt"
    print(final(coladjust( (concat(map (\x -> reassigner x 1) (assigner ((words(input))) 1 )))) 1 (concat(map (\x -> reassigner x 1) (assigner ((words(input))) 1 )))))