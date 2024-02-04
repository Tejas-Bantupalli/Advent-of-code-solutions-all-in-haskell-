import Data.Char
import Data.List

filterer :: [String]->[[String]]
filterer [] = []
filterer (a:b:c:d:rest) = [[a,b,c,d]] ++ filterer rest


lookUp :: String -> [[String]] -> [[String]]
lookUp x y = filter (\z -> (head(z)) == ((x \\ "(),"))) y 

funct :: Int -> [[String]] -> [[String]] ->String -> String -> Int
funct a b x (c) p 
 | length(c)==0 = funct a b x p p 
 | (nub(map (\k -> ((last(init(last(k))))=='Z')) b) == [True]) || (nub(map (\k -> (last(init(last(init(k)))))=='Z') b) == [True]) =(a+1)
 | head(c)=='R' = funct (a+1) (map (\z -> head(lookUp (last(z)) x)) b) x (tail(c))  p
 | head(c)=='L' = funct (a+1) (map (\z -> head(lookUp (last(init(z))) x)) b) x (tail(c)) p
 

main :: IO ()
main = do

    input <- readFile "message.txt"
    print(funct 0 (filter (\z->last(head(z))=='A') (filterer(tail((words(input)))))) ((filterer(tail(words(input))))) (head((words(input)))) (head((words(input)))))