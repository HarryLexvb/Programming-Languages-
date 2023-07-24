-- function myMod
myMod :: Int -> Int -> Int
myMod x y = x - (div x y) * y

-- aCipher : receives an integer returns True if the integer is between 0 and 9.
aCipher :: Int -> Bool
aCipher x = x >= 0 && x <= 9

-- secondNum : given a list of integers returns its second element.
secondNum :: [Int] -> Int
secondNum (x : y : xs) = y

-- orderNum : given two integers, it orders them from smallest to largest.
orderNum :: Int -> Int -> (Int, Int)
orderNum x y = if x < y then (x, y) else (y, x)

-- isMultiple : given an integer n returns True if n is a multiple of 2.
isMultiple :: Int -> Bool
isMultiple n = myMod n 2 == 0

-- greaterNumbers : which given a set of integers returns a set of Boolean values indicating whether each of the integers is greater than 3. For example: greater3:(1; 4; 3) = (False;True; False)
greaterNumbers :: (Int, Int, Int) -> (Bool, Bool, Bool)
greaterNumbers (x, y, z) = (x > 3, y > 3, z > 3)

-- del_position_n: given a list of integers and the position of any element, return a new list without that element in the n-th position. Example of use:
-- funcion take
myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake _ [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

-- funcion drop
myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop _ [] = []
myDrop n (x : xs) = myDrop (n - 1) xs

del_position_n :: [Int] -> Int -> [Int]
del_position_n xs n = myTake (n - 1) xs ++ myDrop n xs

{-
Implement a function that receives a list of integers (not necessarily sorted) and returns an ordered list (in increasing order), consisting only of the odd numbers in the received list.
> myOdd [3,6,4,8,1,9,7]

[1,3,7,9]
-}
-- recursive quicksort function
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

-- funcion que recibe una lista de enteros y retorna una lista ordenada de forma creciente
myOdd :: [Int] -> [Int]
myOdd [] = []
myOdd (x : xs) = if (mod x 2 == 0) then myOdd xs else quicksort (x : myOdd xs)

{-
Implement a function that searches for start substrings. Always consider the n-first characters that the user passes on the command line. Example:

> search_sub :: String → [String] → [String]

> search_sub “an” [“freddy mercury”, “antonio banderas”, “zorro”, “zebra” ]

[“antonio banderas”]

> search_sub “z” [“freddy mercury”, “antonio banderas”, “zorro”, “zebra” ]

[“zorro”, “zebra”]
-}
-- funcion sequential_search
sequential_search :: String -> String -> Bool
sequential_search [] _ = True
sequential_search _ [] = False
sequential_search (x : xs) (y : ys) = if (x == y) then sequential_search xs ys else sequential_search (x : xs) ys

-- funcion search_sub
search_sub :: String -> [String] -> [String]
search_sub _ [] = []
search_sub s (x : xs) = if (sequential_search s x) then x : search_sub s xs else search_sub s xs

-- main
main = do
  putStrLn ("exercise 1")
  print (aCipher 5)
  print (aCipher 10)
  print (aCipher (-1))

  putStrLn ("")
  putStrLn ("exercise 2")
  print (secondNum [1, 2, 3, 4, 5])
  print (secondNum [3, 4, 7, 8, 2, 1])

  putStrLn ("")
  putStrLn ("exercise 3")
  print (orderNum 5 3)
  print (orderNum 3 5)
  print (orderNum 10 5)

  putStrLn ("")
  putStrLn ("exercise 4")
  print (isMultiple 4)
  print (isMultiple 5)

  putStrLn ("")
  putStrLn ("exercise 5")
  print (greaterNumbers (1, 4, 3))
  print (greaterNumbers (5, 4, 3))

  putStrLn ("")
  putStrLn ("exercise 6")
  print (del_position_n [1, 3, 4, 1, 3, 2] 4)
  print (del_position_n [1, 3, 4, 1, 3, 2] 1)

  putStrLn ("")
  putStrLn ("exercise 7")
  print (myOdd [3, 6, 4, 8, 1, 9, 7])
  print (myOdd [2, 4, 6, 8, 10])

  putStrLn ("")
  putStrLn ("exercise 8")
  print (search_sub "an" ["freddy mercury", "antonio banderas", "zorro", "zebra"])
  print (search_sub "z" ["freddy mercury", "antonio banderas", "zorro", "zebra"])
  print (search_sub "z" ["freddy mercury", "antonio banderas", "zorro", "zebra", "zancudo", "zapato"])