import Data.Char
import Data.List
priority_order :: [(Char,Int)]
priority_order = [('A',1), ('K',2), ('Q',3), ('T',4), ('9',5), ('8',6), ('7',7), ('6',8), ('5',9), ('4',10), ('3',11), ('2',12), ('J',13)]

classifier :: String -> String -> (Int,String)
classifier x y
 | x == "JJJJJ" = (1,y)
 | 'J' `elem` x = classifier ((x \\ "JJJJJ") ++ (take (length x - length (x\\"JJJJJ")) (repeat(mostCommon(x \\ "JJJJJ"))))) y
 | (filterer x x) == [0,0,0,0,0]   = (1,y)
 | ((filterer x x) `elem` permutations [1,1,1,1,4]) = (2,y)
 | ((filterer x x) `elem` permutations [2,2,2,3,3])  = (3,y)
 |(filterer x x) `elem` permutations [2,2,2,4,4]  = (4,y)
 |(filterer x x) `elem` permutations [3,3,3,3,4] = (5,y)
 |(filterer x x) `elem` permutations [3,3,4,4,4]  = (6,y)
 | otherwise = (7,y)
 
arranger :: [(String,String)] -> [((Int,String),String)]
arranger [] = []
arranger ((a,b):rest) = [((classifier a a) ,b)] ++ arranger rest

stringer :: String ->Int->Int
stringer [] x = 0
stringer a 0 = 0
stringer (a:as) n = x*14^(n-1) + stringer as (n-1) where x = snd(head([z | z<-priority_order, fst(z) == a]))

comparer :: [((Int,String),String)] -> [((Int,Int),String)]
comparer [] =[]
comparer (((a,b),c):rest) = [((a,(stringer b 5)),c)] ++ comparer rest

splitter :: [((Int,Int),String)] -> Int -> [[((Int,Int),String)]]
splitter x 0 = []
splitter x n = [sortBy fun z]  ++ splitter x (n-1) where z = [z | z<-x, fst (fst z) == n]

fun :: ((Int,Int),String) -> ((Int,Int),String) -> Ordering 
fun a b 
 | snd(fst(a)) < snd(fst(b)) = GT
 |snd(fst(a)) == snd(fst(b)) = EQ
 | otherwise = LT


filterer :: String -> String -> [Int]
filterer [] x=[]
filterer (x:xs) y = [length(filter (/=x) (y))] ++ filterer xs y




final :: [((Int,Int),String)] -> Int ->Int
final [] n = 0
final a 0 =0
final (x:xs) n = n* read(snd(x)) + final xs (n-1)

concater :: [[((Int,Int),String)]] -> [((Int,Int),String)]
concater [] =[]
concater (x:xs) = x ++ concater xs

pairformer :: [String] ->[(String,String)]
pairformer [] = []
pairformer (a:b:c) = [(a,b)] ++ pairformer c

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

finalfinal :: [(String,String)] -> Int
finalfinal x = final (reverse(concat((((splitter (comparer(arranger(x))) 7)))))) (length(x))

main :: IO ()
main = do

    input <- readFile "Day-7txt.txt"


    print(finalfinal(pairformer(words(input))))