import Hugs.Prelude 
import DataFile
letterCheck x = (65<= fromEnum x && fromEnum x <= 90 ) || (97<= fromEnum x  && fromEnum x <=122 )

wordToken :: [Char] -> [[Char]]
wordToken x = wordTokenHelper x "" [] 

wordTokenHelper [] y z = if y/="" then z++[y] else z
wordTokenHelper (x:xs) y z =                   if letterCheck x then wordTokenHelper xs (y++[x]) z
              				                   else if x == ' ' then if y/="" then wordTokenHelper xs "" (z++[y]) else wordTokenHelper xs "" z
							                   else if y/="" then wordTokenHelper xs "" (z++[y]++[[x]]) else wordTokenHelper xs "" (z++[[x]])

wordTokenList :: [String] -> [String]
wordTokenList [] = []											   
wordTokenList (x:xs) = (wordToken x ++ wordTokenList xs)

uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams xs |length xs<2 = []
                 |otherwise = removeduplicates(uniqueBigramsHelper xs)
				 
uniqueBigramsHelper (a:b:xs) =  [(a,b)]++(uniqueBigrams (b:xs))

uniqueBigramsHelper2 (a:b:xs) =  [(a,b)]++(uniqueBigrams2 (b:xs))
							   
uniqueBigrams2 xs |length xs<2 = []
                  |otherwise = uniqueBigramsHelper2 xs					   

bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq xs= bigramsFreqHelper (uniqueBigrams2 xs) (uniqueBigrams2 xs)

bigramsFreqHelper [] _=[]
bigramsFreqHelper (x:xs) l = if elem x xs then bigramsFreqHelper xs l else  ([(x, countFreq x l)]++bigramsFreqHelper xs l)

uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams xs |length xs<3 = []
                  |otherwise = removeduplicates(uniqueTrigramsHelper xs)

uniqueTrigramsHelper (a:b:c:xs) = [(a,b,c)]++(uniqueTrigrams (b:c:xs))

uniqueTrigramsHelper2 (a:b:c:xs) = [(a,b,c)]++(uniqueTrigrams2 (b:c:xs))
								  
								  
uniqueTrigrams2 xs |length xs<3 = []
                   |otherwise = uniqueTrigramsHelper2 xs
				  
trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq xs= trigramsFreqHelper (uniqueTrigrams2 xs) (uniqueTrigrams2 xs)

trigramsFreqHelper [] _=[]
trigramsFreqHelper (x:xs) l = if elem x xs then trigramsFreqHelper xs l else ([(x, countFreq x l)]++trigramsFreqHelper xs l)

countFreq x1 []=0
countFreq x1 (x2:xs) | x1==x2 = 1+ countFreq x1 xs
                     | otherwise = countFreq x1 xs

removeduplicates []=[]				 
removeduplicates (x:xs)= if elem x xs then removeduplicates xs else (x:removeduplicates xs) 

getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b							
getFreq a []= 0
getFreq a ((b,c):xs) | a==b = c
                     | otherwise= getFreq a xs

generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb _ [] =0
generateOneProb  ((x,x1,h),f) (((x2,x3),f1):xs) =  if x==x2 && x1==x3 then f/f1 else generateOneProb ((x,x1,h),f) xs

genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] _ = [] 
genProbPairs (((x),f):xs) l = ([((x),(generateOneProb ((x),f) l))]++ genProbPairs xs l)

generateNextWord x xs = generateNextWordHelper x xs []

generateNextWordHelper _ [] l = if l==[] then error( "Sorry, it is not possible to infer from current database") else l!!(randomZeroToX ((length l)-1))
generateNextWordHelper [w1,w2] (((x1,x2,x3),p):xs) l = if (w1==x1 && w2==x2 && p>0.03) then generateNextWordHelper [w1,w2] xs (l++[x3]) else generateNextWordHelper [w1,w2] xs l

generateText :: String -> Int -> String
generateText x n =   toString (generateTextHelper (wordToken x) n) 

generateTextHelper x 0 = x
generateTextHelper (x:xs) n  =  generateTextHelper ((x:xs)++[(generateNextWord [((x:xs)!!(length(x:xs)-2)),((x:xs)!!(length(x:xs)-1))] (genProbPairs (trigramsFreq(wordTokenList docs)) (bigramsFreq (wordTokenList docs))))]) (n-1) 

toString [] =""
toString (x:xs)=if xs/=[] then if notElem ((xs!!0)!!0) punct then (x++" "++toString xs) else (x++toString xs) else (x++toString xs)

sentToken:: String -> [String]
sentToken x = sentTokenHelper x ""

sentTokenHelper [] _=[]
sentTokenHelper (x:xs) z= if elem x  ['.','!','?'] then ([z++[x]]++(sentTokenHelper xs "")) else if (z=="" && x==' ') then sentTokenHelper xs z else sentTokenHelper xs (z++[x])