import System.Environment ( getArgs )

-- Function Main for the project
main :: IO ()
main = getArgs >>= execFromArgs

-- Other functions for the project
startPush :: [String] -> ([a], [a]) -> ([a], [a])
startPush [] ([], []) = ([],[])
startPush [] b = b
startPush a ([], []) = ([], [])
startPush (a:as) b = startPush as (parserSwap a b)

checkSorted :: (Ord a) => ([a], [a]) -> Bool
checkSorted ([], []) = True
checkSorted ([a], []) = True
checkSorted (a, b) | not (null b) = False
checkSorted (a:as:ac, []) = a <= as && checkSorted (as:ac, [])

checkArgs :: [a] -> ([a], [a])
checkArgs [] = ([], [])
checkArgs x = (x, [])

showResult :: (Ord a) => ([a], [a]) -> [String] -> String
showResult ([], []) [] = "OK"
showResult as []
    | checkSorted as = "OK"
    | otherwise = "KO: "
showResult a b
    | checkSorted (startPush b a) = "OK"
    | otherwise = "KO"

execFromArgs :: [String] -> IO ()
execFromArgs args = getLine >>= putStrLn . showResult
    (checkArgs args) . words

parserSwap :: String -> ([a], [a]) -> ([a], [a])
parserSwap str (a, b)
    | str == "sa" =  saCheck (a, b)
    | str == "sb" =  sbCheck (a, b)
    | str == "sc" =  scCheck (a, b)
    | str == "pa" =  paCheck (a, b)
    | str == "pb" =  pbCheck (a, b)
    | str == "ra" =  raCheck (a, b)
    | str == "rb" =  rbCheck (a, b)
    | str == "rr" =  rrCheck (a, b)
    | str == "rra" = rraCheck (a, b)
    | str == "rrb" = rrbCheck (a, b)
    | str == "rrr" = rrrCheck (a, b)
    | otherwise  = (a, b)

myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend (x:a) b = x:myAppend a b
myAppend [] (x:b) = x:myAppend [] b

-- all rules functions for Pushswap
saCheck :: ([a], [a]) -> ([a], [a])
saCheck (a, b) = case (a, b) of (a:b:as, c) -> (b:a:as, c)

sbCheck :: ([a], [a]) -> ([a], [a])
sbCheck (a, b) = case (a, b) of (c, a:b:as) -> (c, b:a:as)

scCheck :: ([a], [a]) -> ([a], [a])
scCheck (a, b) = case (a, b) of (c:d:bs, a:b:as) -> (d:c:bs, b:a:as)

paCheck :: ([a], [a]) -> ([a], [a])
paCheck (a, b) = case (a, b) of (a, c:d) -> (c:a, d)

pbCheck :: ([a], [a]) -> ([a], [a])
pbCheck (a, b) = case (a, b) of (a:b, c) -> (b, a:c)

raCheck :: ([a], [a]) -> ([a], [a])
raCheck (a:as, b) = case (a, b) of (a, b) -> (myAppend as [a], b)

rbCheck :: ([a], [a]) -> ([a], [a])
rbCheck (a, b:as) = case (a, b) of (a, b) -> (a, myAppend as [b])
rrCheck :: ([a], [a]) -> ([a], [a])
rrCheck (a:as, b:bs) = case (a, b) of (a, b) -> (myAppend as [a],
                                                    myAppend bs [b])

rraCheck :: ([a], [a]) -> ([a], [a])
rraCheck (a,b) = case (a,b) of
    (a, b) -> (init (reverse (myAppend (reverse a) [last a])), b)

rrbCheck :: ([a], [a]) -> ([a], [a])
rrbCheck (a,b) = case (a,b)
    of (a, b) -> (a, init (reverse (myAppend (reverse b) [last b])))

rrrCheck :: ([a], [a]) -> ([a], [a])
rrrCheck (a,b) = case (a,b)
    of (a, b) -> (init (reverse
            (myAppend (reverse a) [last a])),
                init (reverse (myAppend (reverse b) [last b])))