mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int  -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int
myAbs x
    | x > 0 = x
    | x < 0 = x * (-1)

myMin :: Int -> Int -> Int
myMin x y
    | x < y = x
    | x > y = y

myMax :: Int -> Int -> Int
myMax x y
    | x > y = x
    | x < y = y

myTuple :: a -> b -> (a, b)
myTuple x y = (x,y)

myTruple :: a -> b -> c -> (a, b , c)
myTruple x y z = (x,y,z)

myFst :: (a, b) -> a
myFst (x,y) = x

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a,b) = (b, a)

myHead :: [y] -> y
myHead (x:xs) = x
myHead [] = error "Empty list"

myTail :: [y] -> [y]
myTail (x:xs) = xs
myTail [] = error "Empty list"

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength [] = 0

myNth :: [a] -> Int -> a
myNth x index
    | index < 0 = error "Negative number"
    | index > myLength x = error "Index too large"
    | otherwise = myNth (myTail x) (index - 1)

myTake :: Int -> [a] -> [a]
myTake n _
    | n <= 0 =  []
myTake _ [] =  []
myTake n (x:xs) =  x : myTake (n-1) xs

myDrop :: Int -> [a] -> [a]
myDrop n x
    | n <= 0 = x
myDrop _[] = []
myDrop n (_:x) = myDrop (n-1) x

myAppend :: [a] -> [a] -> [a]
myAppend (x:xs) y = x : myAppend xs y
myAppend [] x = x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (i:a) = myAppend (myReverse a) [i]

myInit :: [a] -> [a]
myInit [x] = []
myInit [] = error "List is empty"
myInit (x:xs) = x : myInit xs

myLast :: [a] -> a
myLast [] = error "List is empty"
myLast [x] = x
myLast (x:xs) = myLast xs

myZip :: [a] -> [b] -> [(a, b)]
myZip x [] = []
myZip [] y = []
myZip (a:x) (b:y) = (a,b) : myZip x y

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([] , [])
myUnzip ((a, b):i) = (a: myFst stock, b: mySnd stock)
    where stock = myUnzip i

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _stock [] = []
myFilter stock (x:xs)
    | stock x = x : myFilter stock xs
    | otherwise = myFilter stock xs

myFoldl :: (b-> a -> b) -> b -> [a] -> b
myFoldl stock b [] = b
myFoldl stock b(a:as) = myFoldl stock (stock b a) as

myFoldr :: (a-> b -> b) -> b -> [a] -> b
myFoldr stock b [] = b
myFoldr stock _b (a:as) = stock a (myFoldr stock _b as)