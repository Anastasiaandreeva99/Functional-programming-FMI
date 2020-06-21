--zadacha 2 i 0 -3 tochki
group :: Eq a => [a] -> [[a]]
group = groupBy (==)

combine :: a -> a -> [a]
combine x y = x:(y:[])

addListToList :: [a] -> [[a]] -> [[a]]
addListToList l ll = l:ll

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f [] = []
groupBy f l = reverse(groupByHelp f l [])

groupByHelp :: (a -> a -> Bool) -> [a] -> [[a]] -> [[a]]
groupByHelp f [] ll = ll
groupByHelp f (x:y:l) ll = if (f x y) then (groupByHelp f l newll) else (groupByHelp f l newll2)
        where newll = addListToList (combine x y) ll 
              newll2 = addListToList [y] (addListToList [x] ll)
groupByHelp f (x:[]) ll = addListToList [x] ll
--------------------------------------------------------------------------------------------------------------------------------------------------------------
--zadacha1 - 1 tochka

insert ::  (a -> a -> Ordering)-> a -> [a] -> [a]
insert f x [] = [x]
insert f x (y:ys)
    | (f x y)== LT = x:y:ys
    |(f x y)== EQ = x:y:ys
    | otherwise = y:(insert f x ys)

sortBy ::(a -> a -> Ordering) -> [a] -> [a]
sortBy f [] = []
sortBy f (x:xs) = insert f x (sortBy f xs)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--on
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on ff f x y = ff (f x)  (f y)
-- &&&
(&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&&&) f ff x = ((f x),(ff x))
----------------------------------------------------------------------------------------------------
--sortOn-2 points
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f x = map snd finaltuples
    where  ftuples = map (f &&& id) x
           finaltuples = sortBy (compare `on` fst) ftuples
-----------------------------------------------------------------------------------------------------
--groupon- 2 points
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f x = map functionMap finaltuples
     where ftuples = map (id &&& f) x
           finaltuples = groupBy ((==) `on`snd) ftuples
           functionMap = (map fst)
----------------------------------------------------------------------------------------------------------------------
--classify on - 2 points
classifyOnHelp ::Ord b =>(a-> b) ->[[a]] -> [[a]] ->[[a]]
classifyOnHelp f (x:[]) newl = (x:newl)
classifyOnHelp f (x:y:xs) newl
    |(compare (f (head x)) (f (head y))) ==EQ = classifyOnHelp f ((x++y):xs) newl 
    |otherwise = classifyOnHelp f (y:xs) (x:newl)

classifyOn :: Ord b => (a -> b) -> [a] -> [[a]]
classifyOn f x =classifyOnHelp f (groupOn f sorted) []
    where sorted = sortOn f x
          grouped =  groupOn f sorted
--------------------------------------------------------------------------------------------------------------------------------

data NonEmpty a = a :| [a] deriving (Show)

convertNonEmpty :: [a] -> NonEmpty a
convertNonEmpty (b:c) = b :| c

groupNonEmpty :: Eq a => [a] -> [NonEmpty a]
groupNonEmpty = groupByNonEmpty (==)

groupByNonEmpty :: (a -> a -> Bool) -> [a] -> [NonEmpty a]
groupByNonEmpty f l = map convertNonEmpty  (groupBy f l)

groupOnNonEmpty :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOnNonEmpty f l = map convertNonEmpty  (groupOn f l)


classifyOnNonEmpty :: Ord b => (a -> b) -> [a] -> [NonEmpty a]
classifyOnNonEmpty f l = map convertNonEmpty  (classifyOn f l)
