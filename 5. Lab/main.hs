-- (i) subtraction of two natural numbers
negateNumber :: Int -> Int
negateNumber 0 = 0
negateNumber n = negateNumber (n - 1) - 1

minus :: Int -> Int -> Int
minus x y = if x < y then 0 else x + negateNumber y

-- (ii) greatest common divisor of two natural numbers
mygcd :: Int -> Int -> Int
mygcd x y = if x == y then x else if x > y then mygcd (minus x y) y else mygcd x (minus y x)

-- function mydiv
mydiv :: Int -> Int -> Int
mydiv x y = if x == y then 1 else if x > y then 1 + mydiv (minus x y) y else mydiv x (minus y x)

-- multiplication function
mymul :: Int -> Int -> Int
mymul x y = if x == 0 || y == 0 then 0 else if x == 1 then y else if y == 1 then x else x + mymul x (minus y 1)

-- (iii) least common multiply of two natural numbers
mylcm :: Int -> Int -> Int
mylcm x y = (mymul x y) `mydiv` (mygcd x y)

-- main
main = do
  print ("excercise 1")
  print (minus 5 2) -- 3
  print (minus 2 5) -- 0
  print ("")
  print ("excercise 2")
  print (mygcd 5 2) -- 1
  print (mygcd 2 5) -- 1
  print ("")
  print ("excercise 3")
  print (mylcm 5 2) -- 10
  print (mylcm 2 5) -- 10