
myLast :: [a] -> a
myLast (h:tl) = 
    if null tl
        then h
    else myLast tl
    
myButLast :: [a] -> a
myButLast (h:(s:tl)) = 
    if null tl
        then h
    else myButLast tl

elementAt :: [a] -> Int -> a
elementAt (h:tl) i = 
    if i == 1
        then h
    else elementAt tl (i-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (h:tl) = 
    if null tl
        then 1
    else 1 + (myLength tl)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (h:tl) = 
    if null tl
        then [h]
    else (myReverse tl) ++ [h]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome lst = 
    lst == (myReverse lst)
    
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (h:t) =
    if null t
        then [h]
    else if h == (head t)
        then compress t
    else [h] ++ (compress t)

--Come back for review
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

encode :: (Eq a) => [a] -> (Int, a)
encode lst =  
    
