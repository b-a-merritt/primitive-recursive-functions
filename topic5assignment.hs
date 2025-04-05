main = do
  putStrLn "TEST ADDITION:"
  putStrLn (" 0  +  0  =  0 | " ++ showNat (add (convInt 0) (convInt 0)))
  putStrLn (" 0  +  1  =  1 | " ++ showNat (add (convInt 1) (convInt 0)))
  putStrLn (" 1  +  1  =  2 | " ++ showNat (add (convInt 1) (convInt 1)))
  putStrLn ("54 + 789 = 843 | " ++ showNat (add (convInt 54) (convInt 789)))

  putStrLn "\nTEST SUBTRACTION:"
  putStrLn (" 0  -  0  =  0 | " ++ showNat (sub (convInt 0) (convInt 0)))
  putStrLn (" 1  -  0  =  1 | " ++ showNat (sub (convInt 1) (convInt 0)))
  putStrLn (" 1  -  1  =  0 | " ++ showNat (sub (convInt 1) (convInt 1)))
  putStrLn ("54  - 32  = 22 | " ++ showNat (sub (convInt 54) (convInt 32)))

  -- putStrLn "\nTEST MULTIPLICATION:"
  -- putStrLn (" 0  *  0  =  0 | " ++ show (mul 0 0))
  -- putStrLn (" 1  *  0  =  0 | " ++ show (mul 1 0))
  -- putStrLn (" 0  *  1  =  0 | " ++ show (mul 0 1))
  -- putStrLn (" 1  *  1  =  1 | " ++ show (mul 1 1))
  -- putStrLn (" 2  *  1  =  2 | " ++ show (mul 2 1))
  -- putStrLn ("54  *  3 = 162 | " ++ show (mul 54 3))

  -- putStrLn "\nTEST DIVISION:"
  -- putStrLn (" 0  %  1  =  0 | " ++ show (divd 0 1))
  -- putStrLn (" 1  %  1  =  1 | " ++ show (divd 1 1))
  -- putStrLn (" 2  %  1  =  2 | " ++ show (divd 2 1))
  -- putStrLn ("54  %  3  = 18 | " ++ show (divd 54 3))

data Natural = Zero | Successor Natural

-- 
recursiveNatural :: a -> (Natural -> a -> a) -> Natural -> a
recursiveNatural arg _ Zero = arg
recursiveNatural arg base (Successor acc) = base acc (recursiveNatural arg base acc)

add :: Natural -> Natural -> Natural
add a b = recursiveNatural b (\ _ c -> Successor c) a

sub :: Natural -> Natural -> Natural
sub a b = recursiveNatural b (\_ c -> Successor c) a

-- add :: Integer -> Integer -> Integer
-- add 0 n = n
-- add m n = add (m - 1) (n + 1)

-- sub :: Integer -> Integer -> Integer
-- sub x 0 = x
-- sub 0 _ = 0
-- sub x y = sub (x - 1) (y - 1)

-- mul :: Integer -> Integer -> Integer
-- mul 0 _ = 0
-- mul m n = add n (mul (m - 1) n)

-- divd :: Integer -> Integer -> Integer
-- divd _ 0 = error "Division by zero"
-- divd n m
--   | n < m     = 0
--   | otherwise = 1 + divd (sub n m) m

--------------------- UTILITIES -------------------------

-- converts an Integer into a Natural
convInt :: Integer -> Natural
convInt a
  | a <= 0 = Zero
  | otherwise = Successor (convInt (a - 1))

-- converts a Natural to a String
showNat :: Natural -> String
showNat Zero = "0"
showNat (Successor n) = show (convNatural n + 1)

-- converts a Natural back to an Integer
convNatural :: Natural -> Int
convNatural Zero = 0
convNatural (Successor n) = 1 + convNatural n