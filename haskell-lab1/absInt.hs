absInt :: Int -> Int -- absInt 2 = absInt (-2) = 2
absInt x = if x < 0
	   then -x
	   else x
	
