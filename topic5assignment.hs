main = do
  putStrLn "TEST ADDITION:"
  putStrLn (" 0  +  0  =  0 | " ++ show (add 0 0))
  putStrLn (" 1  +  0  =  1 | " ++ show (add 1 0))
  putStrLn (" 0  +  1  =  1 | " ++ show (add 0 1))
  putStrLn (" 1  +  1  =  2 | " ++ show (add 1 1))
  putStrLn ("54 + 789 = 843 | " ++ show (add 54 789))

  putStrLn "\nTEST SUBTRACTION:"
  putStrLn (" 0  -  0  =  0 | " ++ show (sub 0 0))
  putStrLn (" 1  -  0  =  1 | " ++ show (sub 1 0))
  putStrLn (" 0  -  1  = -1 | " ++ show (sub 0 1))
  putStrLn (" 1  -  1  =  0 | " ++ show (sub 1 1))
  putStrLn ("54  - 32  = 22 | " ++ show (sub 54 32))

  putStrLn "\nTEST MULTIPLICATION:"
  putStrLn (" 0  *  0  =  0 | " ++ show (mul 0 0))
  putStrLn (" 1  *  0  =  0 | " ++ show (mul 1 0))
  putStrLn (" 0  *  1  =  0 | " ++ show (mul 0 1))
  putStrLn (" 1  *  1  =  1 | " ++ show (mul 1 1))
  putStrLn (" 2  *  1  =  2 | " ++ show (mul 2 1))
  putStrLn ("54  *  3 = 162 | " ++ show (mul 54 3))

  putStrLn "\nTEST DIVISION:"
  putStrLn (" 0  %  1  =  0 | " ++ show (divd 0 1))
  putStrLn (" 1  %  1  =  1 | " ++ show (divd 1 1))
  putStrLn (" 2  %  1  =  2 | " ++ show (divd 2 1))
  putStrLn ("54  %  3  = 18 | " ++ show (divd 54 3))

data Nat = Zero | Succ Nat

fromInteger :: Integer -> Nat
fromInteger n
  | n <= 0 = Zero
  | otherwise = Succ (fromInteger (n - 1))

add :: Integer -> Integer -> Integer
add 0 n = n
add m n = add (m - 1) (n + 1)

sub :: Integer -> Integer -> Integer
sub x 0 = x
sub 0 _ = 0
sub x y = sub (x - 1) (y - 1)

mul :: Integer -> Integer -> Integer
mul 0 _ = 0
mul m n = add n (mul (m - 1) n)

divd :: Integer -> Integer -> Integer
divd _ 0 = error "Division by zero"
divd n m
  | n < m     = 0
  | otherwise = 1 + divd (sub n m) m