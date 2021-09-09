import Data.Char

myElem :: Eq a => a -> [a] -> Bool
myElem _[] = False
myElem x (y:ys) = x == y || myElem x ys

safeDiv :: Int -> Int -> Maybe Int
safeDiv n m = if m == 0 then Nothing
        else Just (n `div` m)

safeNth :: [a] -> Int -> Maybe a
safeNth (x:xs) 0 = Just x
safeNth (x:xs) n
        | n < 0 = Nothing
        | otherwise = safeNth xs (n-1)
safeNth [] 0 = Nothing
safeNth x xs = Nothing

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc x = fmap succ x

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _stock [] =  Nothing
myLookup stock ((x,y):xys)
        | stock == x =  Just y
        | otherwise = myLookup stock xys

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing
maybeDo _ _ Nothing = Nothing
maybeDo a b c = Just a <*> b <*> c

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt string
        | all (`myElem` "0123456789") string = Just (read string :: Int)
        | otherwise = Nothing

getLineLength :: IO Int
getLineLength = getLine >>= return.length

printAndGetLength :: String -> IO Int
printAndGetLength string = putStr string >>
                putStr "\n" >>
                return (length string)