{-
Construct a function del_position_n :: [Int] → Int → [Int] in which, given a list of integers and the
position of any element, return a new list without that element at the n-th position.
position.

Example usage:
> del_position_n [1,3,4,1,3,2] 4
[1,3,4,3,2]
> del_position_n [1,3,4,1,3,2] 1
[3,4,1,3,2]
-}

-- implement the function delete element in a list
myDelete :: [Int] -> Int -> [Int]
myDelete [] _ = []
myDelete (x : xs) n = if (n == 1) then xs else x : myDelete xs (n - 1)

-- function that receives a list of integers and the position of any element and returns a new list without the element in the n-th position
del_position_n :: [Int] -> Int -> [Int]
del_position_n [] _ = []
del_position_n (x : xs) n = if (n == 1) then myDelete (x : xs) n else x : del_position_n xs (n - 1)

{-
Implement a function that receives a list of integers (not necessarily sorted) and returns
an ordered list (in increasing order), consisting only of the odd numbers of the received list.
received.
> odd [3,6,4,8,1,9,7]
[1,3,7,9]
-}

-- recursive quicksort function
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

-- function that receives a list of integers and returns an ordered list in increasing order
myOdd :: [Int] -> [Int]
myOdd [] = []
myOdd (x : xs) = if (mod x 2 == 0) then myOdd xs else quicksort (x : myOdd xs)

{-
Construct a function that returns the first n elements of the Fibonacci sequence.
> fibonacci 10
[0,1,1,2,3,5,8,13,21,34]
-}

fibonacci :: Int -> [Int]
fibonacci 0 = []
fibonacci 1 = [0]
fibonacci 2 = [0, 1]
fibonacci n = fibonacci (n - 1) ++ [fibonacci (n - 1) !! (n - 2) + fibonacci (n - 1) !! (n - 3)]

{-
 Implement a function that calculates the intersection between 2 lists.
> intersection [3,6,5,7] [9,7,5,1,3]
[3,5,7]
-}

-- function myelem
myelem :: Int -> [Int] -> Bool
myelem _ [] = False
myelem n (x : xs) = if (n == x) then True else myelem n xs

intersection :: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection _ [] = []
intersection (x : xs) (y : ys) = if (myelem x (y : ys)) then x : intersection xs (y : ys) else intersection xs (y : ys)

{-
Implement a function that searches for start substrings. Always consider the first n characters that the user passes on the command line. Example:
> search_sub :: String → [String] → [String]
> search_sub “an” [“freddy mercury”, “antonio banderas”, “zorro”, “zebra” ]
[“antonio banderas”]
> search_sub “z” [“freddy mercury”, “antonio banderas”, “zorro”, “zebra” ]
[“zorro”, “zebra”]
-}

-- sequential search function
sequential_search :: String -> String -> Bool
sequential_search [] _ = True
sequential_search _ [] = False
sequential_search (x : xs) (y : ys) = if (x == y) then sequential_search xs ys else sequential_search (x : xs) ys

-- funcion busca_sub
search_sub :: String -> [String] -> [String]
search_sub _ [] = []
search_sub s (x : xs) = if (sequential_search s x) then x : search_sub s xs else search_sub s xs

{-
Define a function that repeats occurrences up to a certain value, in the format of a list, such that
list, such that:
> repeats :: Int → [Int]
> repeats 4
[4,4,4,4,3,3,3,2,2,1]
-}

-- function myconcatMap
myconcatMap :: (a -> [b]) -> [a] -> [b]
myconcatMap f [] = []
myconcatMap f (x : xs) = f x ++ myconcatMap f xs

-- funcion myreplicate
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n - 1) x

-- fuincion repeats
repeats :: Int -> [Int]
repeats n = myconcatMap (\x -> myreplicate x x) [n, n - 1 .. 1]

{-
make a haskell function to determine if the content of a list is a palindrome. The return of this function must be True
or False. Example:
> palindrome [1,2,3,3,4,5]
False
> palindrome [1,2,3,2,2,1] False
True
-}

-- myreverse function
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x : xs) = myreverse xs ++ [x]

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == myreverse xs

-- main
main = do
  putStrLn ("exercise 1")
  print (del_position_n [1, 3, 4, 1, 3, 2] 4)
  print (del_position_n [1, 3, 4, 1, 3, 2] 1)

  putStrLn ("")
  putStrLn ("exercise 2")
  print (myOdd [3, 6, 4, 8, 1, 9, 7])
  print (myOdd [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

  putStrLn ("")
  putStrLn ("exercise 3")
  print (fibonacci 10)
  print (fibonacci 5)

  putStrLn ("")
  putStrLn ("exercise 4")
  print (intersection [3, 6, 5, 7] [9, 7, 5, 1, 3])
  print (intersection [1, 2, 3, 4, 5] [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

  putStrLn ("")
  putStrLn ("exercise 5")
  print (search_sub "an" ["freddy mercury", "antonio banderas", "zorro", "zebra"])
  print (search_sub "z" ["freddy mercury", "antonio banderas", "zorro", "zebra"])

  putStrLn ("")
  putStrLn ("exercise 6")
  print (repeats 4)
  print (repeats 5)

  putStrLn ("")
  putStrLn ("exercise 7")
  print (palindrome [1, 2, 3, 4, 5])
  print (palindrome [1, 2, 3, 2, 1])
