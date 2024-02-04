import Data.Char
pairformer :: [String] -> [String] ->[(String,String)]
pairformer [] [] = []
pairformer (a:as) (b:bs) =  [(a,b)] ++ pairformer as bs

listformer :: Int -> Int -> [Int]
listformer a 0 = [0]
listformer a n = [(a-n)*n] ++ listformer a (n-1) 


calculator :: [(String,String)] -> Int
calculator [] = 1
calculator (a:as) =  length([x | x <- listformer (read x) (read x), x>=(read y) ])*(calculator as) where
    x = fst(a)
    y=snd(a)

converter :: String -> Int -> Int
converter x 0 = ord(head(x)) - ord('0')
converter (x:xs) y = (ord(x) - ord('0'))*10^y + converter xs (y-1)


final :: [String] -> Int
final a = calculator(pairformer (tail(fst(break (=="Distance:") ((a))))) (tail(snd(break (=="Distance:") ((a))))))

main :: IO ()
main = do

    input <- readFile "Day-6_txt.txt"


    print(final(words(input)))