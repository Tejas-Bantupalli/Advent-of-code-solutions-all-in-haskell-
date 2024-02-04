import Data.Char
pairformer :: [String] ->[(String,String)]
pairformer [] = []
pairformer (a:b:c) = [(a,b)] ++ pairformer c


gamechanger :: [(String,String)] -> Bool
gamechanger [] = True
gamechanger ((a,b):c) 
 | b== "blue" && converter(a++":")<=14 = gamechanger c 
 | b== "blue;" && converter(a++":")<=14 = gamechanger c 
 | b== "blue," && converter(a++":")<=14 = gamechanger c 
 | b== "green," && converter(a++":")<=13 = gamechanger c 
 | b== "green;" && converter(a++":")<=13 = gamechanger c 
 | b== "green" && converter(a++":")<=13 = gamechanger c 
 | b== "red," && converter(a++":")<=12 = gamechanger c 
 | b== "red;" && converter(a++":")<=12 = gamechanger c 
 | b== "red" && converter(a++":")<=12 = gamechanger c 
 | otherwise = False

splitter :: [String] -> [[String]]
splitter(a:[]) = [[a]]
splitter [] = [[]]
splitter (x:y:rest)
 | x=="Game" = [x:head(splitter (y:rest))] ++ tail(splitter(y:rest))
 |y=="Game" = [[x]] ++ splitter(y:rest)
 |otherwise = [x:head(splitter (y:rest))] ++ tail(splitter(y:rest))

final :: [[String]] -> Int
final [] = 0
final (x:xs)
 | gamechanger(pairformer(tail(tail(x)))) == True = converter(snd(head(pairformer(x)))) + final(xs)
 | otherwise = final(xs)


converter :: String -> Int
converter x
 | length x == 2 = ord(head(x)) - ord('0')
 | length x == 3 = (ord(head(x))-ord('0'))*10 + ord(head(tail(x))) - ord('0')
 |length x == 4 = 100

main :: IO ()
main = do

    input <- readFile "Day-2_txt.txt"


    print(final(splitter(words(input))))