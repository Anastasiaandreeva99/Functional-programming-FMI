import Prelude hiding (Show)
import Data.List (nub, inits, tails, sortOn)
findMax :: [Int] -> Int
findMax [] = 0
findMax [x] = x
findMax (x:y:xs) = if x > y then findMax (x:xs) else findMax (y:xs)
listOfMax :: [[Int]] -> [Int]
listOfMax [] = []
listOfMax (x:xs) = findMax x : listOfMax xs
findMin :: [Int] -> Int
findMin [] = 0
findMin [x] = x
findMin (x:y:xs) = if x < y then findMin (x:xs) else findMin (y:xs)
listOfMin :: [[Int]] -> [Int]
listOfMin [] = []
listOfMin (x:xs) = findMin x : listOfMin xs
extremum :: [[Int]] -> Int
extremum [] = 0
extremum xs = let 
    (m:ms) = listOfMax xs
    (n:ns) = listOfMin xs
    in if all (== m) ms then m else if all (== n) ns then n else 0
--equallist :: [Int] -> [Int] -> Bool
--equallist [] _ = True
--equallist (x:x1:xs) (y:y1:ys)  = if x == x1 || x == y1 then equallist (x1:xs) (y1:ys) else False
type Name = String
type StartTime = (Int,Int)
type Length = Int
type Show = (Name,StartTime,Length)
bigger :: Show -> Show -> Bool
bigger (_,b,_) (_,d,_) = if (fst b) < (fst d) then True else if (fst b) == (fst d) && (snd b) < (snd d) then True else False
endsBefore :: Show -> Show -> Bool
endsBefore (_,b,c) (_,d,_) = if ((fst b) * 60 + (snd b) + c ) < ((fst d) * 60 + (snd d)) then True else False
isProgram :: [Show] -> Bool
isProgram [] = False
isProgram [x] = True
isProgram (x:y:xs) = if bigger x y && endsBefore x y then isProgram (y:xs) else False
generate :: [Show] -> Int -> [[Show]]
generate xs 2 = [[r1,r2] | r1@(a,_,_) <- xs , r2@(b,_,_) <- xs, a/=b ]
generate xs n = [r1:r2 | r1<- xs , r2<- generate xs (n - 1)] 
diversestProgram :: [Show] -> [Show]
--diversestProgram [] = (,,)
subsequences :: [a] -> [[a]]
subsequences l = [] : [suffix | prefix <- inits l,
                                suffix <- tails prefix,
                                not $ null suffix]
diversestProgram ls = fst $ head maxLengthProgram
    where newList = filter (\x -> isProgram x) (filter (\x -> (length x) > 1) (subsequences ls))
          newListMap = map (\x -> (x, (length x))) newList
          maxLength = maximum (map snd newListMap)
          maxLengthProgram = filter (\x -> (snd x) == maxLength) newListMap

fib :: Int -> Int -> [Int]
fib a b = a : fib b (a + b)

--makeList :: Int -> Int ->
makeFibonachi :: Int-> Int ->[Int]
makeFibonachi a b = a : (makeFibonachi b (a + b))

makeListHelpProd :: Int -> Int -> [Int] -> [Int]
makeListHelpProd el n lst= el : (makeListHelpProd getLastN n (lst ++ [getLastN]))
    where  getLastN
            | (length lst == 1) = el * el
            | length lst < n = product lst
            | otherwise = product (take n (reverse lst))

makeListProd :: Int -> Int -> [Int]
makeListProd a b = makeListHelpProd a b [a]

makeListHelpSum :: Int -> Int -> [Int] -> [Int]
makeListHelpSum el n lst= el : (makeListHelpSum getLastN n (lst ++ [getLastN]))
    where  getLastN
            | (length lst == 1) = el
            | length lst < n = sum lst
            | otherwise = sum (take n (reverse lst))

makeListSum :: Int -> Int -> [Int]
makeListSum a b = makeListHelpSum a b [a]

