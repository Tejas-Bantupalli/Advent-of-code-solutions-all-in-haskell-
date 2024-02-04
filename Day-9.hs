differences :: [Int]-> [Int]
differences []  = []
differences x
 | length(x)==1 = []
differences (x:y:xs) = [y-x] ++ (differences (y:xs))  

listofd :: [Int]->[[Int]]
listofd x 
 | x /= take (length(x)) (repeat 0) = [differences x] ++ listofd (differences x)
 | otherwise = []


final :: [[Int]] -> [Int] -> Int 
final [] y = head(y)
final (x:xs) y 
 | y==take (length(y)) (repeat 0) = final ((xs)) ([last(x)]++x)
 |otherwise =final xs ([head(x)-head(y)]++x) 




finalfinal :: [[String]] -> Int 
finalfinal [] = 0
finalfinal (x:xs) = (final (tail(reverse(listofd(map (\y -> read y::Int) x)) ++ [map (\y->read y::Int) x])) (head(reverse(listofd(map (\y -> read y::Int) x))))) + finalfinal xs


initialmapper :: [String] ->[[String]]
initialmapper [] = []
initialmapper x = [fst(splitAt 21 x)] ++ initialmapper (snd(splitAt 21 x))



main :: IO ()
main = do

    input <- readFile "random_shit.txt"
    print(finalfinal(initialmapper(words(input))))