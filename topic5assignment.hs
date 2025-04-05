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

  putStrLn "\nTEST MULTIPLICATION:"
  putStrLn (" 0  *  0  =  0 | " ++ showNat (mul (convInt 0) (convInt 0)))
  putStrLn (" 1  *  0  =  0 | " ++ showNat (mul (convInt 1) (convInt 0)))
  putStrLn (" 0  *  1  =  0 | " ++ showNat (mul (convInt 0) (convInt 1)))
  putStrLn (" 1  *  1  =  1 | " ++ showNat (mul (convInt 1) (convInt 1)))
  putStrLn (" 2  *  1  =  2 | " ++ showNat (mul (convInt 2) (convInt 1)))
  putStrLn ("54  *  3 = 162 | " ++ showNat (mul (convInt 54) (convInt 3)))

  putStrLn "\nTEST DIVISION:"
  putStrLn (" 0  %  1  =  0 | " ++ showNat (divd (convInt 0) (convInt 1)))
  putStrLn (" 1  %  1  =  1 | " ++ showNat (divd (convInt 1) (convInt 1)))
  putStrLn (" 2  %  1  =  2 | " ++ showNat (divd (convInt 2) (convInt 1)))
  putStrLn ("54  %  3  = 18 | " ++ showNat (divd (convInt 54) (convInt 3)))

data Natural = Zero | Successor Natural

-- 
recursiveNatural :: arg -> (Natural -> arg -> arg) -> Natural -> arg
recursiveNatural arg _ Zero = arg
recursiveNatural arg base (Successor acc) = base acc (recursiveNatural arg base acc)

predecessorNatural :: Natural -> Natural
predecessorNatural Zero = Zero
predecessorNatural (Successor a) = a

add :: Natural -> Natural -> Natural
add a b = recursiveNatural b (\_ c -> Successor c) a

sub :: Natural -> Natural -> Natural
sub a = recursiveNatural a (\_ c -> predecessorNatural c)

mul :: Natural -> Natural -> Natural
mul a b = recursiveNatural Zero (\_ c -> add b c) a

divd :: Natural -> Natural -> Natural
divd _ Zero = error "division by zero error"
divd dividend divisor = 
  if convNatural dividend < convNatural divisor
    then Zero
    else Successor (divd (sub dividend divisor) divisor)


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