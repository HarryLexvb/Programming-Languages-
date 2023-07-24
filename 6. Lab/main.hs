-- myeven function
myeven :: Int -> Bool
myeven 0 = True
myeven n = myodd (n - 1)

-- myodd function
myodd :: Int -> Bool
myodd 0 = False
myodd n = myeven (n - 1)

-- myfilter function
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x : xs)
  | f x = x : myfilter f xs
  | otherwise = myfilter f xs

-- myhead function, dont use head function prelude
myhead :: [a] -> a
myhead [] = error "empty list"
myhead (x : _) = x -- x is the first element of the list

-- mytail function
mytail :: [a] -> [a]
mytail [] = error "empty list"
mytail (_ : xs) = xs -- xs is the list without the first element

sortEven :: [Int] -> [Int]
sortEven xs =
  let evens = myfilter myeven xs -- filter the even numbers
      sortedEvens = quicksort evens -- sort the even numbers
   in replaceEvens xs sortedEvens -- replace the even numbers in the original list

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) =
  quicksort [y | y <- xs, y <= x]
    ++ [x]
    ++ quicksort [y | y <- xs, y > x]

replaceEvens :: [Int] -> [Int] -> [Int]
replaceEvens [] _ = []
replaceEvens (x : xs) evens -- replace the even numbers in the original list
  | myeven x = (myhead evens) : replaceEvens xs (mytail evens) -- replace the even number
  | otherwise = x : replaceEvens xs evens -- keep the odd number

-- mylength function
mylength :: [a] -> Int
mylength [] = 0
mylength (_ : xs) = 1 + mylength xs

count_similarities :: String -> Int
count_similarities s = count_similarities_Helper s 0 (mylength s - 1)

count_similarities_Helper :: String -> Int -> Int -> Int
count_similarities_Helper s start end
  | start > end = 0 -- base case
  | start == end = if s !! start == s !! end then 1 else 0 -- base case
  | otherwise = (if s !! start == s !! end then 1 else 0) + count_similarities_Helper s (start + 1) (end - 1) -- recursive case,

-- main
main = do
  putStrLn ("sort even numbers, if the list contains odd numbers, keep them in the same position")
  print (sortEven [2, 3, 1, 8, 4, 10])
  print (sortEven [4, 6, 3, 1, 2, 3, 8])

  putStrLn ("")

  putStrLn ("count similarities in a string")
  putStrLn $ "zorrarearroz tiene " ++ show (count_similarities "zorrarearroz") ++ " similitudes"
  putStrLn $ "arroraa tiene " ++ show (count_similarities "arroraa") ++ " similitudes"
  putStrLn $ "keeppeep tiene " ++ show (count_similarities "keeppeek") ++ " similitudes"
  putStrLn $ "keep6peek tiene " ++ show (count_similarities "keep6peek") ++ " similitudes"
  putStrLn $ "ke6ppeek tiene " ++ show (count_similarities "ke6ppeek") ++ " similitudes"
