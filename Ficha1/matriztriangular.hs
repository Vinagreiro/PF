triSup :: Mat a -> Bool
triSup m = triSupAux [0..] m

triSupAux :: [Int] -> Mat a -> Bool
triSupAux _ [] = True
triSupAux (i:is) (l:ls) = if (nZeros i l)==True then triSupAux is ls else False

nZeros :: Int -> [a] -> Bool
nZeros 0 _ = True
nZeros n (x:xs) | n>0 && x==0 = nZeros (n-1) xs
                | otherwise = False