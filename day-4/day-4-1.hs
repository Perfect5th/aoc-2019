
minVal = 264360
maxVal = 746325

main = do
    print $ length $ filter validPw [show pw | pw <- [minVal..maxVal]]

validPw pw = hasDouble pw && increasingOrder pw

hasDouble (d1:[]) = False
hasDouble (d1:d2:ds) = d1 == d2 || hasDouble (d2 : ds)

increasingOrder (d1:[]) = True
increasingOrder (d1:d2:ds) = d1 <= d2 && increasingOrder (d2 : ds)
