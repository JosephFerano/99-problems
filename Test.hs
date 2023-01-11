myLast [] = error "Cannot accept an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast x
  | length x < 2 = error "Cannot accept an empty list"
  | otherwise = case x of
                  [x,y] -> x
                  (x:xs) -> myButLast xs

elementAt (x:xs) i = if i == 0 then x else elementAt xs (i - 1)

myLength [] = 0
myLength xs = foldl (\acc _ -> acc + 1) 0 xs
