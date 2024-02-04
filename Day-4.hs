import Data.List
splitter :: [String] -> [[String]]
splitter(a:[]) = [[a]]
splitter [] = [[]]
splitter (x:y:rest)
 | x=="Card" = [x:head(splitter (y:rest))] ++ tail(splitter(y:rest))
 |y=="Card" = [[x]] ++ splitter(y:rest)
 |otherwise = [x:head(splitter (y:rest))] ++ tail(splitter(y:rest))


breaker :: [String] -> ([String],[String])
breaker a = break (=="|") a

final :: [[String]] -> Int 
final [] = 0
final (x:xs) 
 | (length(snd(breaker x) \\ fst(breaker x))) /=length(snd(breaker x)) = 2^(length(snd(breaker x)) - (length(snd(breaker x) \\ fst(breaker x))) -1 ) + final xs
 |otherwise =final xs

main :: IO ()
main = do

    input <- readFile "Day-4.txt"


    print(final(splitter(words(input))))