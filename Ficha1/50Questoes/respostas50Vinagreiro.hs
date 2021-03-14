module Cinquenta where

import Data.Char
import Data.List
import Data.Either

--Questao 1
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | y>x = x:(enumFromTo (x+1) y)
                | x==y = [x]
                | otherwise = []

--Questao 2
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | z>x = x:(enumFromThenTo' (x+(y-x)) (y+(y-x)) z)
                      | z==x = [x]
                      | otherwise = []

--Questao 3
maisMais' :: [a] -> [a] -> [a]
maisMais' l [] = l
maisMais' [] l = l
maisMais' (x:xs) (y:ys) = x:(maisMais' xs (y:ys))

--Questao 4
exclamacoes :: [a] -> Int -> a
exclamacoes (x:xs) n | n==0 = x
                     | n>0 = (exclamacoes xs (n-1))
                     | otherwise = error "tente outra vez"

--Questao 5
reverse' :: [a] -> [a]
reverse' (x:xs) = (reverse' xs) ++ [x]

--Questao 6
take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x:xs) | n>0 = x:(take' (n-1) xs)
               | otherwise = []

--Questao 7
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs) | n>0 = (drop' (n-1) xs)
               | otherwise = (x:xs)

--Questao 8
zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' l [] = []
zip' [] l = []
zip' (x:xs) (y:ys) = (x,y):(zip' xs ys)

--Questao 9
elem' :: Eq a => a -> [a] -> Bool
elem' n [] = False
elem' n (x:xs) | n==x = True
               | otherwise = (elem' n xs)

--Questao 10
replicate' :: Int -> a -> [a]
replicate' n a | n>0 = a:(replicate' (n-1) a)
               | otherwise = []
               
--Questao 11
intersperse' :: a -> [a] -> [a]
intersperse' n [] = []
intersperse' n (x:[]) = [x]
intersperse' n (x:xs) = x:n:(intersperse' n xs)

--Questao 12
group' :: Eq a => [a] -> [[a]]
group' [] = [[]]
group' (x:xs) = (agrupa [x] xs)
               where agrupa :: [a] -> [a] -> [[a]]
                     agrupa x [] = [x]
                     agrupa x (y:ys) | y==x = (agrupa (y:x) ys)
                                     | otherwise = x:(agrupa [y] ys)

--Questao 13
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:[]) = x
concat' (x:xs) = x ++ (concat' xs)

--Questao 14
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' (x:[]) = [[],[x]]
inits' l = inits' (init l) ++ [l]

--Questao 15
tails' :: [a] ->[[a]]
tails' [] = [[]]
tails' (x:[]) = [[x],[]]
tails' l = [l] ++ tails'(tail l)

--Questao 16
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' (x:xs) (y:ys) | x==y = isPrefixOf' xs ys
                          | otherwise = False

--Questao 17
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' l1 l2 | (last l1)==(last l2) = isSuffixOf' (init l1) (init l2)
                  | otherwise = False
                  
--Questao 18
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] [] = True
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) | x==y = isSubsequenceOf' xs ys
                               | otherwise = isSubsequenceOf' (x:xs) ys

--Questao 19
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' n (x:xs) = aux 0 n (x:xs)
                       where aux a n [] = []
                             aux a n (x:xs) | n==x = a:(aux (a+1) n xs)
                                            | otherwise = (aux (a+1) n xs)

--Questao 20
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) | elem' x xs = nub' xs
            | otherwise = x:(nub' xs)

--Questao 21
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs) | n==x = xs
                 | otherwise = x:(delete' n xs)

--Questao 22
barraBarra :: Eq a => [a] -> [a] -> [a]
barraBarra [] _ = []
barraBarra l [] = l
barraBarra (x:xs) (y:ys) = barraBarra (retira y (x:xs)) ys
                          where retira n [] = []
                                retira n (x:xs) | n==x = xs
                                                | otherwise = x:(retira n xs)
                                                
--Questao 23
union' :: Eq a => [a] -> [a] -> [a]
union' [] [] = []
union' l [] = l
union' [] l = l
union' (x:xs) (y:ys) | elem y (x:xs) = union' (x:xs) ys
                     | otherwise = (union' (x:xs) ys) ++ [y]

--Questao 24
intersperse' :: Eq a => [a] -> [a] - [a]
intersperse' [] [] = []
intersperse' l [] = []
intersperse' [] l = []
intersperse' (x:xs) (y:ys) | elem x (y:ys) = x:(intersperse' xs (y:ys))
                           | otherwise = intersperse' xs (y:ys)

--Questao 25
insert' :: Ord a => a -> [a] -[a]
insert' n [] = [n]
insert' n (x:xs) | n>=x = x:(insert' n xs)
                 | otherwise n:(x:xs)
             
--Questao 26
unwords' :: [String] -> String
unwords' [] = " "
unwords' (x:[]) = x
unwords' (x:xs) = x++" "++(unwords' xs)
                
--Questao 27
unlines' :: [String] -> String
unlines' [] = " "
unlines' (x:[]) = x
unlines' (x:xs) = x++"\n"++(unlines' xs)

--Questao 28
pMaior' :: Ord a => [a] -> Int
pMaior' (x:[]) = 0
pMaior' (x:y:xs) = qualMaior (x:y:xs)
                  where qualMaior [] = 0
                        qualMaior (x:y:xs) | x>=y = qualMaior (x:xs)
                                           | otherwise = 1 + (qualMaior (y:xs))

--Questao 29
temRepetidos' :: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (x:[]) = False
temRepetidos' (x:y:xs) | elemm x xs = True
                       | otherwise = temRepetidos' xs
                                     where elemm n [] = False
                                           elemm n (x:xs) | n==x = True
                                                          | otherwise = elemm n xs

--Questao 30
algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (x:xs) | elem' x ['0'..'9'] = x:(algarismos' xs)
                   | otherwise = algarismos' xs

--Questao 31
posImpares' :: [a] -> [a]
posImpares' [] = []
posImpares' (x:[]) = []
posImpares' (x:y:xs) = y:(posImpares' xs)

--Questao 32
posPares' :: [a] -> [a]
posPares' [] = []
posPares' (x:[]) = [x]
posPares' (x:y:xs) = x:(posPares' xs)

--Questao 33
isSorted' :: Ord a => [a] -> Bool
isSorted' [] = True
isSorted' (x:[]) = True
isSorted' (x:y:xs) | x<=y = isSorted' (y:xs)
                   | otherwise = False
                   
--Questao 34
iSort' :: Orda => [a] -> [a]
iSort' [] = []
iSort' (x:[]) = [x]
iSort' (x:xs) = insert' x (iSort' xs)

--Questao 35
menor' :: String -> String -> Bool
menor' _ [] = False
menor' [] _ = True
menor' (x:xs) (y:ys) | (ord x) < (ord y) = True
                     | (ord x) > (ord y) = False
                     | otherwise = menor' xs ys

--Questao 36
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' a ((x,y):xs) | a==x = True
                       | otherwise = elemMSet' a xs

--Questao 37
lengthMSet' :: [(a,Int)] -> Int
lengthMSet' [] = 0
lengthMSet' ((x,y):xs) = y + (lengthMSet' xs)

--Questao 38
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,y):xs) = replica (x,y) ++ (converteMSet' xs)
                          where replica (x,0) = []
                                replica (x,y) = [x] ++ replica (x ,(y-1))

--Questao 39
insereMSet' :: Eq => a -> [(a,Int)] -> [(a,Int)]
insereMSet' n [] = [(n,1)]
insereMSet' n ((x,y):xs) | n==x = ((x,(y+1)):xs)
                         | otherwise = (x,y):(insereMSet' n xs)

--Questao 40
removeSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeSet' _ [] = []
removeSet' n ((x,y):xs) | n==x && y>1 = ((x,(y-1)):xs)
                        | n==x && y==1 = xs
                        | otherwise = (x,y):(removeSet' n xs)

--Questao 41
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (x:xs) = contador 1 (x:xs)
                       where contador n [] = []
                             contador n (x:[]) = [(x:n)]
                             contador n (x:y:xs) | x==y = contador (n+1) (y:xs)
                                                 | otherwise = (x,n) : (contador 1 (y:xs))

--Questao 42
--partitionEithers :: [Either a b] -> ([a],[b])

--Questao 43
catMaybes' :: [Maybe a] -> [a]
catMaybes' (Just a : xs) = a:(catMaybes' xs)
catMaybes' (Nothing xs) = catMaybes' xs

--Questao 44
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (Norte:xs) = posicao (x,(y+1)) xs
posicao (x,y) (Sul:xs) = posicao (x,(y-1)) xs
posicao (x,y) (Este:xs) = posicao ((x+1),y) xs
posicao (x,y) (Oeste:xs) = posicao ((x-1),y) xs

--Questao 45
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2) | x1==x2 && y1==y2 = []
                        | x1<x2 = [Este] ++ caminho ((x1+1),y1) (x2,y2)
                        | x1>x2 = [Oeste] ++ caminho ((x1-1),y1) (x2,y2)
                        | y1<y2 = [Norte] ++ caminho (x1,(y1+1)) (x2,y2)
                        | y1>y2 = [Sul] ++ caminho (x1,(y1-1)) (x2,y2)

--Questao 46
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show
    
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (Norte:xs) = vertical xs
vertical (Sul:xs) = vertical xs
vertical (Oeste:xs) = False
vertical (Este:xs) = False

--Questao 47
data Posicao = Pos Int Int
               deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral ((Pos x y):[]) = (Pos x y)
maisCentral ((Pos x1 y1):(Pos x2 y2):xs) | dist' (Pos x1 y1) <= dist' (Pos x2 y2) = maisCentral ((Pos x1 y1):xs)
                                         | dist' (Pos x1 y1) > dist' (Pos x2 y2) = maisCentral ((Pos x2 y2):xs)
                                                                                   where dist' (Pos x y) = sqrt(fromIntegral(y^2 + x^2))

--Questao 48
data Posicao = Pos Int Int
               deriving Show

vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' (Pos x y) ((Pos x1 y1):xs) | x==x1 && y==(y1+1) = (Pos x1 y1):(vizinhos' (Pos x y) xs)
                                     | x==x1 && y==(y1-1) = (Pos x1 y1):(vizinhos' (Pos x y) xs)
                                     | y==y1 && x==(x1+1) = (Pos x1 y1):(vizinhos' (Pos x y) xs)
                                     | y==y1 && x==(x1-1) = (Pos x1 y1):(vizinhos' (Pos x y) xs)
                                     | otherwise = vizinhos' (Pos x y) xs

--Questao 49
mesmaOrdenada' :: [Posicao] -> Bool
mesmaOrdenada' ((Pos x y):[]) = True
mesmaOrdenada' ((Pos x y):(Pos x1 y1):xs) | y==y1 = mesmaOrdenada' ((Pos x1 y1):xs)
                                          | otherwise = False

--Questao 50
data Semaforo = Verde | Amarelo | Vermelho
                deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK l = (aux l) <= 1
                  where aux [Vermelho] = 0
                        aux [Verde] = 1
                        aux [Amarelo] = 1
                        aux (Vermelho:xs) = aux xs
                        aux (Amarelo:xs) = 1 + aux xs
                        aux (Vermelho:xs) = 1 + aux xs