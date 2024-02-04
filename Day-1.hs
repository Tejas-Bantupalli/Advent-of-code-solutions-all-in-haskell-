import Data.Char
array :: [(String,Int)]
array = [("one",1),("two",2),("three",3),("four",4),("five",5),("six",6),("seven",7),("eight",8),("nine",9)]
a:: [Int] 
a = []

list :: [(Char,Int)]
list = [('1',1),('2',2),('3',3),('4',4),('5',5),('6',6),('7',7),('8',8),('9',9)]

removeMaybe :: Maybe Int -> [Int]
removeMaybe (Just x) = [x]

checker :: String -> [Int]
checker [] = []
checker c@(x:xs)
 | lookup (take 3 c) array /= Nothing = a ++ removeMaybe (lookup (take 3 c) array)++ checker (drop 2 c)
 | lookup (take 4 c) array /= Nothing =a ++ removeMaybe (lookup (take 4 c) array)++ checker (drop 3 c)
 |lookup (take 5 c) array /= Nothing = a ++ removeMaybe (lookup (take 5 c) array) ++ checker (drop 4 c)
 | lookup (head c) list /= Nothing = a++ removeMaybe (lookup (head c) list) ++ checker xs
 | otherwise = checker xs


final :: [String] -> Int 
final [] = 0
final (x:xs) =head(checker x) * 10 + last(checker x) + final xs



main :: IO ()
main = do

    input <- readFile "Day-1_text.txt"


    print(final(words(input)))



