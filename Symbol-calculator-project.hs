import Data.Char
import Prelude hiding (newstring)
data Tree = Empty| Node String Tree Tree deriving (Eq,Show)
type MyPair = (String,String)
{-Example input(the input should be without spaces):
calculator
x=5
y=9
x+y/4
command:evaluate
-the program returns trees -in evaluate the result is tree-leaf and teh result is the roots
-}
{-!!! commands :
evaluate-evaluate the expression
build - print the tree of the expression
reduce -reduce simple expressions 
def -deferenciate simple expressions example: "y+x*2" = 1+2 ; x^2 = 2*x; "y+x*2" = 1+2*x
brackets - get multiple divider out of scope
domin - make simple expressions with the same dеnominator
-}
list1=[("x","2"),("y","5")]
exampledef= def list1 (buildTree "y+x^2")
examplereduce = reduce (buildTree "4/6+6/4")
examplebrackets = commandBrackets (buildTree "2+4")
exampleeval = evaluate list1 (buildTree "x+y*2")
examplebuil = buildTree "2*(x+y)+8+6"
--To start the program run the function calculator
calculator  = readFromConsole [] ""
readFromConsole :: [MyPair] ->String -> IO() -- readFromConsole from the console 
readFromConsole xs exp = do  
    line <- getLine  
    if (isCommand line) 
        then  print(startCommand line exp xs) 
        else do  
            if (isVariable line) then readFromConsole (([(head line)],[(last line)]):xs) exp
            else if(isExpression line) then readFromConsole xs line else  return()
--helpfunctions for the readFromConsole 
isVariable :: String -> Bool
isVariable input = isAlpha (head input) && (head (drop 1 input)) == '='

isExpression :: String -> Bool
isExpression input = getNumberAlpha input/=[] && not (isVariable input)

isCommand :: String -> Bool
isCommand input =   (take 8 input) == "command:"
    where command = "command:"

getCommand :: String -> String 
getCommand input = drop (length "command:") input

startCommand :: String -> String -> [MyPair] -> Tree
startCommand com expr l 
    |c == "build" = buildTree expr
    |c == "evaluate" = snd  (evaluate l (buildTree expr))
    |c == "reduce" = reduce (buildTree expr)
    |c == "def" = snd(def l (buildTree expr))
    |c == "brackets" = commandBrackets (buildTree expr)
    |c == "domin" = fst(domin  (buildTree expr) l)
        where c = getCommand com
--building the tree 
buildTree :: String -> Tree
buildTree inp = (snd (buildTree1 inp Empty))

buildTree1 :: String -> Tree  -> (String,Tree)
buildTree1 "" tree1 =  ("empty", tree1)
buildTree1 (x:xs)  tree1 
    |(isDigit x || isAlpha x) && ( take 1  xs == ['+'] ||  take 1 xs == ['-'])= buildTree1 xs (Node [x] Empty Empty)
    |isDigit x || isAlpha x = buildTree1 newstring1 (snd (buildPrtree (getExpr(x:xs)) Empty))
    |elem x "(" = buildTree1 newstring3 (getExpr2 xs) 
    |elem x "*/^" && ( take 1 xs == "(" ) = buildTree1 newstring2 (buildTree2 tree1 (getExpr2 (drop 1 xs)) [x])
    |elem x "+-" && ( take 1 xs == "(" ) = buildTree1 newstring2 (buildTree2 tree1 (getExpr2 (drop 1 xs)) [x])
    |elem x "+-" = buildTree1 newstring (buildTree2 tree1 newtree [x])
        where newtree = (snd (buildPrtree (getExpr xs) Empty))
              newstring = drop (length(getExpr xs)) xs
              newstring1 = drop (length(getExpr (x:xs))) (x:xs)
              newstring2= drop (getLengthExpr xs + 1) xs
              newstring3 = drop (getLengthExpr xs + 1) xs
--Help functions to build treee
buildMiniTree :: String -> String -> String -> Tree -- building basic treee -"+" "3" "4"
buildMiniTree x y z = (Node y tree1 tree2)
    where tree1 = (Node x Empty Empty)
          tree2 = (Node z Empty Empty)

buildTree2 :: Tree -> Tree -> String -> Tree -- building tree from two Trees and string for operation
buildTree2 tree1 tree2 op = (Node op tree1 tree2)

buildTree3 :: Tree-> String -> String -> Tree -- building tree with string string and tree 
buildTree3 tree1 str op = (Node op tree1 (Node str Empty Empty))

buildPrtree :: String -> Tree -> (String,Tree) -- builPriorityTree is building tree of type "3*4/5" only with the priority operations
buildPrtree "" tree = ("end",tree)
buildPrtree input tree
    |getNumberAlpha input /= [] && (length (drop (length number1) input)) == 0 = ("empty",(Node number1 Empty Empty))
    |getNumberAlpha input /= [] = buildPrtree newstring1 (buildMiniTree number1 op number2 )
    |getNumberAlpha input == [] = buildPrtree newstring (buildTree3 tree number3 operation) 
        where operation = take 1  input
              number3 = getNumberAlpha (drop 1 input)
              newstring = drop ((length number3)+ 1) input
              number1 = getNumberAlpha input
              op =  take 1 (drop (length number1) input)
              number2 = getNumberAlpha (drop ((length number1) + 1) input)
              newstring1 = drop ((length number1) + (length number2) + 1) input
--help functions 
getNumberAlpha :: String -> String -- help function-get numbers and alphas
getNumberAlpha [] = [] 
getNumberAlpha (x:xs) 
    |isAlpha x = [x]
    |isDigit x = x : getNumberAlpha xs 
    |otherwise =[]

getExpr :: String -> String  
getExpr input = if (take 1 newstring2) == ['('] 
    then init newstring0
    else  newstring1
        where newstring0 = (takeWhile ( \y -> y /= '(') input ) 
              newstring1 = (takeWhile ( \y -> y /= '+' && y /= '-') input)
              newstring2 = (drop (length (takeWhile ( \y -> y /= '+' && y /= '-' && y /= '(') input )) input)

getBrackets:: String -> String
getBrackets input = takeWhile ( \y -> y /= ')') input

getLengthExpr :: String -> Int 
getLengthExpr input = (length (getBrackets input)) + (length (getExpr (drop (length (getBrackets input)+1) input))) 

getExpr2 :: String -> Tree
getExpr2 input 
    |(op ==['*'] || op == ['/'] || op == ['^']) && nextisbracket = (snd (buildTree1 bracketsstring Empty))
    |(op ==['*'] || op == ['/'] || op == ['^'])  =  buildTree2 (snd (buildTree1 bracketsstring Empty)) (snd (buildPrtree stringafterbrackets Empty)) op 
    |otherwise =  (snd (buildTree1 bracketsstring Empty))
     where bracketsstring = getBrackets input 
           stringafterbrackets = getExpr (drop (2 + length (getBrackets input)) input)
           op = take 1 (drop (1 + length (getBrackets input)) input)
           nextisbracket = (take 1 (drop (2 + length (getBrackets input)) input)) == ['(']

stringToNumber :: String -> Int
stringToNumber input =read input :: Int

isLeaf :: Tree -> Bool
isLeaf (Node _ x y) = if x == Empty && y == Empty then True else False

isOp :: String -> Bool
isOp a = a == "+" || a== "-"|| a== "*"|| a== "/"|| a== "^"

power :: Int -> Int -> Int
power x 0 = x 
power x y = power(x * x) (y-1) 


findalpha :: String-> [MyPair] -> [String]
findalpha x =map snd . filter ((==x).fst)

isnumber :: String -> Bool
isnumber [] = False
isnumber (x:xs) 
    |isDigit x = True
    |otherwise = False
--commands
---command evaluate tree
evaluate :: [MyPair] -> Tree -> ([MyPair],Tree)
evaluate xs (Node a x y) 
    | x==Empty && y == Empty = (xs ,(Node a x y))
    | isOp a && isLeaf x && isLeaf y= (xs ,(Node result Empty Empty))
    | isOp a && not (isLeaf x) && not (isLeaf y) = evaluate xs (Node a newtree1 newtree2)
    | isOp a && not (isLeaf x) && isLeaf y = evaluate xs (Node a newtree1 y)
    | isOp a && isLeaf x && not (isLeaf y) = evaluate xs (Node a x newtree2)
        where result = evalTree xs a x y 
              newtree1 = snd (evaluate xs x)
              newtree2 = snd (evaluate xs y)
-- help func to evaluating
evalTree :: [MyPair]->String -> Tree -> Tree -> String
evalTree ys op (Node x Empty Empty) (Node y Empty Empty) 
    |op == ['+'] = show (newx + newy)
    |op == ['-'] = show(newx - newy)
    |op == ['*'] = show(newx * newy)
    |op == ['/'] = show (newx `div` newy)
    |op == ['^'] = show (power newx newy)
        where newx = if not (isnumber x) then stringToNumber (head (findalpha x ys )) else stringToNumber x
              newy = if not (isnumber y) then stringToNumber (head (findalpha y ys)) else stringToNumber y

gcd' :: Int -> Int-> Int--finds biggest common divisor
gcd' a b
      | b == 0     = abs a
      | otherwise  = gcd' b (a `mod` b)

getTreeValue :: Tree -> String
getTreeValue (Node value tree1 tree2) = value

getTreeValueLeftTree :: Tree -> String
getTreeValueLeftTree (Node value tree1 tree2) = getTreeValue tree1

getRightTree :: Tree -> Tree
getRightTree (Node value tree1 tree2) = tree2

getleftTree :: Tree -> Tree
getleftTree (Node a b c)= b

makeReducedTree :: Tree -> Int -> Tree
makeReducedTree (Node value tree1 tree2) k = (Node newvalue tree1 tree2)
    where newvalue = show ((stringToNumber value) `div` (gcd' (stringToNumber value) k))
-- command reduce 
reduce :: Tree -> Tree
reduce (Node a Empty Empty) =  (Node a Empty Empty) 
reduce (Node a (Node c Empty Empty) (Node k Empty Empty))
    |a == "/" && isnumber c && (gcd' (stringToNumber c) (stringToNumber k)) == (stringToNumber k) = (Node newvalue Empty Empty)
    |a == "/" && isnumber c  && (gcd (stringToNumber c) (stringToNumber k)) /= 1  = (Node "/" newvalue0 newvalue1) 
    |a/= "/" = (Node a (Node c Empty Empty) (Node k Empty Empty))
         where  newvalue = show ((stringToNumber c) `div` (stringToNumber k))
                newvalue0 = (Node (show ((stringToNumber c) `div` (gcd' (stringToNumber c) (stringToNumber k)))) Empty Empty)
                newvalue1 = (Node (show ((stringToNumber k) `div` (gcd' (stringToNumber c) (stringToNumber k))))  Empty Empty)
reduce (Node a b c) 
    |a == "/" && ((isLeaf b) == False) && ((isLeaf c) == False) = (Node a (fst (reduceHelp (b, (getTreeValueLeftTree (commandBrackets c))))) (fst (reduceHelp (c, (getTreeValueLeftTree (commandBrackets b))))))
    |a == "/" && ((isLeaf b) == False) && ((isLeaf c) == True) = (Node a (fst(reduceHelp (b,(getTreeValue c)))) c)
reduce (Node a (Node c d e) (Node k Empty Empty)) 
    |a == "/" && c == "*"  && isnumber k && (gcd' (stringToNumber k) (stringToNumber (getTreeValue d))) == 1 && (gcd' (stringToNumber k) (stringToNumber (getTreeValue e))) == 1 =(Node a (reduce (Node c d e))  (Node k Empty Empty))
    |a == "/" && c == "*"  && isnumber k = reduce (Node a (Node c newD newE) (Node (show newK) Empty Empty))
    |otherwise = (Node a (reduce (Node c d e))  (Node k Empty Empty))
        where newD = if (isnumber (getTreeValue d)) then makeReducedTree d (stringToNumber k) else d
              newk = if (isnumber (getTreeValue d)) then (stringToNumber k) `div` (gcd' (stringToNumber k) (stringToNumber(getTreeValue d))) else stringToNumber k
              newE = if newk/=1  then makeReducedTree e (stringToNumber k) else e
              newK = if newE/= e then newk `div`(gcd' (stringToNumber k) (stringToNumber(getTreeValue e))) else newk
reduce (Node a (Node c Empty Empty) (Node k l m)) = (Node a  (Node c Empty Empty) (reduce (Node k l m)))
reduce (Node a (Node c d e) (Node k l m)) 
    |a == "/" && c == "*"  && isnumber k && (gcd' (stringToNumber k) (stringToNumber (getTreeValue d))) == 1 && (gcd' (stringToNumber k) (stringToNumber (getTreeValue e))) == 1 =(Node a (reduce (Node c d e))  (Node k Empty Empty))
    |a == "/" && c == "*"  && isnumber k = reduce (Node a (Node c newD newE) (Node (show newK) l m))
    | otherwise = (Node a (reduce (Node c d e)) (reduce (Node k l m)))
        where newD = makeReducedTree d (stringToNumber k)
              newk = (stringToNumber k) `div` (gcd' (stringToNumber k) (stringToNumber (getTreeValue d)))
              newE = if newk/=1  then makeReducedTree e (stringToNumber k) else e
              newK = if newE/= e then newk `div`(gcd' (stringToNumber k) (stringToNumber (getTreeValue e))) else newk

reduceHelp :: (Tree, String) -> (Tree, String)
reduceHelp ((Node b c d), r)
    | (isOp b) && (isLeaf c) && (isLeaf d)= ((Node b newC newD), newR)
    | (isOp b) && (isLeaf c) == False && (isLeaf d) = ((Node b (fst (reduceHelp (c,r))) newD), newR)
    | (isOp b) = ((Node b (fst (reduceHelp (c,r))) (fst (reduceHelp (d,r)))), r) 
        where newC = (Node (show ((stringToNumber (getTreeValue c)) `div` (gcd' (stringToNumber (getTreeValue c)) (stringToNumber r)))) Empty Empty)
              newR = show ((stringToNumber r) `div` (gcd' (stringToNumber (getTreeValue c)) (stringToNumber r)))
              newD = (Node (show ((stringToNumber (getTreeValue d)) `div` (gcd' (stringToNumber (getTreeValue d)) (stringToNumber r)))) Empty Empty)

--command brackets
commandBrackets :: Tree -> Tree
commandBrackets (Node a Empty Empty) = (Node a Empty Empty)
commandBrackets (Node a b c)
    | (a == "+" || a == "-") && (isLeaf b) == False && (isLeaf c) = if (gcd' (stringToNumber (getTreeValueLeftTree (commandBrackets b))) (stringToNumber (getTreeValue c))) /= 1 then (Node a (Node (getTreeValue (commandBrackets b)) (newgetTreeValueLeftTree) (getRightTree (commandBrackets b))) newC2) else (Node a b c) -- v leviq podnoud tam kydeto vzimamedqsnotot poddyrvo na b e b razdeleno na obshtiq si mnovitel, a ne na obshtiq mnogitel na b i c
    | (a == "+" || a == "-")  && (isLeaf b) && (isLeaf c) = (Node "*" (Node (show(gcd' (stringToNumber (getTreeValue b)) (stringToNumber (getTreeValue c)))) Empty Empty) (Node a newB newC))
        where newB = (makeReducedTree b (gcd' (stringToNumber (getTreeValue b)) (stringToNumber (getTreeValue c))))
              newC = (makeReducedTree c (gcd' (stringToNumber (getTreeValue b)) (stringToNumber (getTreeValue c))))
              newgetTreeValueLeftTree = (Node (show (gcd' (stringToNumber (getTreeValueLeftTree (commandBrackets b))) (stringToNumber (getTreeValue c)))) Empty Empty)
              newC2 = (Node (show ((stringToNumber (getTreeValue c)) `div` (gcd' (stringToNumber (getTreeValueLeftTree (commandBrackets b))) (stringToNumber (getTreeValue c))))) Empty Empty)

mult :: (String, Tree) -> (String, Tree)
mult (x, (Node a Empty Empty)) = (x, (Node (show ((stringToNumber a) * (stringToNumber x))) Empty Empty))
mult (x, (Node a left rigth)) = (x, (Node (show ((stringToNumber a) * (stringToNumber x))) (snd (mult (x, left))) (snd (mult (x, rigth)))))
--command domin
domin :: Tree->[MyPair]-> (Tree,[MyPair])
domin (Node a Empty Empty) lis = ((Node a Empty Empty),lis)
domin (Node a x y) lis
    | isroot && isdivx && (getTreeValue y) =="/" = ((Node a newL newR),lis)
    | isroot && isdivx && ((getTreeValue y) =="+" || (getTreeValue y) == "-") = ((Node a newL (fst(domin y lis))),lis)
    | isroot &&  isdivy && ((getTreeValue x) =="+" || (getTreeValue x) == "-") = ((Node a (fst(domin x lis)) newR),lis)
    | isroot &&  isdivy && (isLeaf x)  = (result1,lis)
    | isroot && isdivx && (isLeaf y)  = (result2,lis)
        where mnozhitelL = show ((stringToNumber (getTreeValue (snd (evaluate lis (getRightTree y))))) `div` (stringToNumber (multiple2 ((getRightTree y),(getRightTree x))))) --trqbwva div da e s obyrnati argumente
              mnozhitelR = show ((stringToNumber (getTreeValue (snd (evaluate lis (getRightTree x))))) `div` (stringToNumber (multiple2 ((getRightTree y),(getRightTree x))))) --trqbva div da e s obyrnati argumenti
              newRL = (snd (mult(mnozhitelR, (getleftTree y))))
              newRR = (snd (mult(mnozhitelR, (getRightTree y))))
              newLL = (snd (mult(mnozhitelL, (getleftTree x)))) -mnozhitelR
              newLR = (snd (mult(mnozhitelL, (getRightTree x)))) 
              newL = (Node "/" newLL newLR)
              newR = (Node "/" newRL newRR)
              isdivx = (getTreeValue x) =="/"
              isdivy = (getTreeValue y) =="/"
              isroot = (a == "+" || a == "-")
              result1 = (Node a (Node (show ( (stringToNumber (multiple1 (getRightTree y))) * (stringToNumber(getTreeValue x)))) Empty Empty) y) -- lqvoto pod dyrvo e s umnovenie 
              result2 = (Node a x (Node (show ( (stringToNumber (multiple1 (getRightTree x))) * (stringToNumber(getTreeValue y)))) Empty Empty))
multiple1 :: Tree -> String
multiple1 (Node a Empty Empty) = a
multiple1 (Node a b c)
    | (a == "+"|| a == "-"|| a == "*") && (isLeaf b) && (isLeaf c) = show (op (stringToNumber (getTreeValue b))  (stringToNumber (getTreeValue c)))
    |  (a == "+"|| a == "-"|| a == "*") && (isLeaf b) == False && (isLeaf c) = show (op (stringToNumber (multiple1 b))  (stringToNumber (getTreeValue c)))
    |  (a == "+"|| a == "-"|| a == "*") && (isLeaf b) && (isLeaf c) == False = show (op (stringToNumber (multiple1 c))  (stringToNumber (getTreeValue b)))
    |  (a == "+"|| a == "-"|| a == "*") && (isLeaf b) == False && (isLeaf c) ==False = show (op (stringToNumber (multiple1 b))  (stringToNumber (multiple1 c)))
            where op  = if(a=="+") then (+) else if (a == "*") then (*) else (-)

multiple2 :: (Tree, Tree) -> String
multiple2 (tree1, tree2) = show result 
    where result = gcd' (stringToNumber (multiple1 tree1)) (stringToNumber (multiple1 tree2))
--command defirintiation -works only with simple expressions
def :: [MyPair] -> Tree -> ([MyPair],Tree)
def l (Node a b c)
    |(a == "+" || a == "-") && isLeaf b && isLeaf c && numberb && numberc = (l,Empty)
    |(a == "+" || a == "-") && isLeaf b && (not (isLeaf c)) && numberb = (def l c)
    |(a == "+" || a == "-") && isLeaf c && (not (isLeaf b)) && numberc = (def l b)
    |(a == "+" || a == "-") && isLeaf b && (not (isLeaf c))  = (l,(Node a (Node "1" Empty Empty) (snd (def l c))))
    |(a == "+" || a == "-") && isLeaf c && (not (isLeaf b))  = (l,(Node a (snd(def l b))  (Node "1" Empty Empty) ))
    |(a == "+" || a == "-") && isLeaf b  && isLeaf c && alphab && alphac= (l,(Node "2" Empty Empty))
    |(a == "+" || a == "-") && isLeaf b  && isLeaf c && ((alphab && numberc) || (alphac && numberb))= (l,(Node "1" Empty Empty))
    |(a == "+" || a == "-") && not (isLeaf b ) && not (isLeaf c)  = (l,(Node a (snd (def l b)) (snd (def l c))))
    |a == "*" && isLeaf b && isLeaf c && ((alphab && numberc) || (alphac && numberb)) = (l,(Node newa Empty Empty))
    |a == "^" && isLeaf b && isLeaf c && numberc = (l,(Node "*" newtree1 newtree2 ))
            where alphab = findalpha (getTreeValue b) l /= []
                  alphac = findalpha (getTreeValue c) l /= []
                  numberb = isnumber (getTreeValue b)
                  numberc = isnumber (getTreeValue c)
                  newa = if numberb then getTreeValue b else getTreeValue c
                  newtree1 = c
                  helpvalue =show((stringToNumber (getTreeValue c)) -1)
                  newtree2 = (Node a b (Node helpvalue Empty Empty )) 
       