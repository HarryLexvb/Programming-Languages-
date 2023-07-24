-- function returning the n-th element of the fibonacci sequence
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- function that deletes the first occurrence of an entry from a list
delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete n (x : xs) = if n == x then xs else x : delete n xs

-- function that, given a list of numbers, returns the list of odd numbers.
myOdd :: [Int] -> [Int]
myOdd [] = []
myOdd (x : xs) = if mod x 2 == 0 then myOdd xs else x : myOdd xs

-- function to calculate the minimum of a list
myMin :: [Int] -> Int
myMin [] = 0
myMin [x] = x
myMin (x : xs) = min x (myMin xs)

-- main
main = do
  putStrLn ("exercise 1")
  print (fibonacci 10)
  putStrLn ("exercise 2")
  print (delete 2 [1, 2, 3, 2, 1])
  putStrLn ("exercise 3")
  print (myOdd [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  putStrLn ("exercise 4")
  print (myMin [10, 2, 45, 6, 7, 8, 9, 10])
