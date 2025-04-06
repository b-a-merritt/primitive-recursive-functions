main = do
  putStrLn "OVERALL ASSIGNMENT TESTING:"
  putStrLn ("add  [\"SSS0\", \"SS0\"] =    SSSSS0 | " ++ add "SSS0" "SS0")
  putStrLn ("mult [\"SSS0\", \"SS0\"] =   SSSSSS0 | " ++ mult "SSS0" "SS0")
  putStrLn ("expo [\"SSS0\", \"SS0\"] = SSSSSSSS0 | " ++ expo "SSS0" "SS0")
  putStrLn ("tsub [\"SS0\", \"SSSS0\"] =      SS0 | " ++ tsub "SS0" "SSSS0")
  putStrLn ("tsub [\"SSSSS0\", \"SSSS0\"] =     0 | " ++ tsub "SSSSS0" "SSSS0")
  putStrLn ("sig [\"SSSSS0\"] =              S0 | " ++ sig "SSSSS0")
  putStrLn ("sig [\"SSS0\"] =                S0 | " ++ sig "SSS0")
  putStrLn ("sig [\"0\"] =                    0 | " ++ sig "0")
  putStrLn ("eq [\"SSS0\", \"SS0\"] =          S0 | " ++ eq "SSS0" "SS0")
  putStrLn ("eq [\"SSS0\", \"SSS0\"] =          0 | " ++ eq "SSS0" "SSS0")
  putStrLn ("p [\"SSSS0\"] =               SSS0 | " ++ toRepresentation(predecessor (fromRepresentation "SSSS0")))
  putStrLn ("p [\"SSSS0\"] =               SSS0 | " ++ toRepresentation(predecessor (fromRepresentation "SSSS0")))

data Natural = Zero | Successor Natural

-- Recursion for natural numbers
-- When the input is Zero, the first arg is returned.
-- The successor function is applied to the inner natural and
-- the recursive result.
recursor :: arg -> (Natural -> arg -> arg) -> Natural -> arg
recursor arg _ Zero = arg
recursor arg base (Successor acc) = base acc (recursor arg base acc)


-- It returns the predecessor of a natural number
-- It stops at Zero, but for Successor n, it returns n
predecessor :: Natural -> Natural
predecessor Zero = Zero
predecessor (Successor arg) = arg

-- Adds two natural numbers
internalAdd :: Natural -> Natural -> Natural
internalAdd summand1 summand2 = recursor summand2 (\_ c -> Successor c) summand1
add :: String -> String -> String
add summand1 summand2 = toRepresentation (internalAdd (fromRepresentation summand1) (fromRepresentation summand2))

-- Subtracts two natural numbers
internalSub :: Natural -> Natural -> Natural
internalSub minuend = recursor minuend (\_ c -> predecessor c)
tsub :: String -> String -> String
tsub minuend subtrahend = toRepresentation (internalSub (fromRepresentation subtrahend) (fromRepresentation minuend))

-- Multiplies two natural numbers
interalMult :: Natural -> Natural -> Natural
interalMult factor1 factor2 = recursor Zero (\_ c -> internalAdd factor2 c) factor1
mult :: String -> String -> String
mult factor1 factor2 = toRepresentation (interalMult (fromRepresentation factor1) (fromRepresentation factor2))

-- Divides two natural numbers (rounding to zero if < 1)
interalDivd :: Natural -> Natural -> Natural
interalDivd _ Zero = error "division by zero error"
interalDivd dividend divisor =
  if convNat dividend < convNat divisor
    then Zero
    else Successor (interalDivd (internalSub dividend divisor) divisor)
divd :: String -> String -> String
divd dividend divisor = toRepresentation (interalDivd (fromRepresentation dividend) (fromRepresentation divisor))

-- Exponentiates two natural numbers
interalExpo :: Natural -> Natural -> Natural
interalExpo base = recursor (Successor Zero) (\_ c -> interalMult base c)
expo :: String -> String -> String
expo base power = toRepresentation (interalExpo (fromRepresentation power) (fromRepresentation base))

sig :: String -> String
sig "0" = "0"
sig _ = "S0"

eq :: String -> String -> String
eq a b =
  if convNat (fromRepresentation a) == convNat (fromRepresentation b)
    then "0"
    else "S0"

--------------------- UTILITIES -------------------------

-- converts a Natural back to an Integer
convNat :: Natural -> Int
convNat Zero = 0
convNat (Successor n) = 1 + convNat n

-- converts a Natural into the string representation for the class
toRepresentation :: Natural -> String
toRepresentation Zero = "0"
toRepresentation (Successor n) = "S" ++ toRepresentation n

-- converts a string representation to a Natural
fromRepresentation :: String -> Natural
fromRepresentation "0" = Zero
fromRepresentation ('S' : rest) = Successor (fromRepresentation rest)