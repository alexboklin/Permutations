permute :: [a] -> [[a]]

--rotate constructs "rotations" of a list, e.g.:
--GHCi> rotate [1,2,3,4]
--[[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]
rotate x = rotate' x [] 0 where
	rotate' src dest n
		| n == length x = dest
		| otherwise = rotate' (tail src ++ [head src]) (dest ++ [src]) (n + 1)

--permTail gives permutations of the list's tail, e.g.:
--GHCi> permTail [1,2,3,4]
--[[1,2,3,4],[1,3,4,2],[1,4,2,3]]
permTail (x:xs) = map (x:) $ rotate xs

permute [] = [[]]
permute x = concat $ map (\x -> permTail x) $ rotate x
