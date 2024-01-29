
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
myLength (h:tl) = 
    if null tl
        then 1
    else 1 + (myLength tl)

myReverse :: [a] -> [a]
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

compress :: [a] -> [a]
compress (h:tl) = 
    (th:tt) = tl
    if 
