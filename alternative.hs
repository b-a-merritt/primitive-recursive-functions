import Debug.Trace

main = do
  putStrLn "OVERALL ASSIGNMENT TESTING:"
  putStrLn ("add  [\"SSS0\", \"SS0\"] =    SSSSS0 | " ++ add ["SSS0", "SS0"])
  putStrLn ("mult [\"SSS0\", \"SS0\"] =   SSSSSS0 | " ++ mult ["SSS0", "SS0"])
  putStrLn ("expo [\"SSS0\", \"SS0\"] = SSSSSSSS0 | " ++ expo ["SSS0", "SS0"])
  putStrLn ("tsub [\"SS0\", \"SSSS0\"] =      SS0 | " ++ tsub ["SS0", "SSSS0"])
  putStrLn ("tsub [\"SSSSS0\", \"SSSS0\"] =     0 | " ++ tsub ["SSSSS0", "SSSS0"])
  putStrLn ("divd [\"SS0\", \"SSSSSS0\"] =   SSS0 | " ++ divd ["SS0", "SSSSSS0"])
  putStrLn ("sig [\"SSSSS0\"] =            True | " ++ show (sig "SSSSS0"))
  putStrLn ("sig [\"SSS0\"] =              True | " ++ show (sig "SSS0"))
  putStrLn ("sig [\"0\"] =                False | " ++ show (sig ['0']))
  putStrLn ("p [\"SSSS0\"] =               SSS0 | " ++ p "SSSS0")
  putStrLn ("p [\"SSSS0\"] =                SS0 | " ++ p "SSS0")

type PN = [Char]
type Vec = [PN]

-- 1) Zero function
z :: Vec -> PN
z x = "0"

-- 2a) Successor function
s :: Vec -> PN
s x = 'S':head x

-- 2b) Predecessor function
p :: PN -> PN
p "0" = "0"
p ('S':xs) = xs

-- 3) Projection function
pr :: Int -> (Vec -> PN)
pr 1 (x:_)    = x
pr i (_:xs)   = pr (i - 1) xs

rho :: (Vec -> PN) -> (Vec -> PN) -> (Vec -> PN)
rho f g = h
    where h ("0":xs) =  f xs
          h (('S':n):xs) =  g $ n:(h (n:xs):xs)

-- 4) Composition
circ :: (Vec -> PN) -> [Vec -> PN] -> (Vec -> PN)
circ f gs x = f $ sequence gs x


-- 5) Addition
add :: Vec -> PN
add = rho fAdd gAdd
fAdd :: Vec -> PN
fAdd = pr 1
gAdd :: Vec -> PN
gAdd = s `circ` [pr 2]

-- 6) Subtraction
tsub :: Vec -> PN
tsub = rho fSub gSub
fSub :: Vec -> PN
fSub = pr 1
gSub :: Vec -> PN
gSub = (\[x] -> p x) `circ` [pr 2]

-- 7) Multiply
mult :: Vec -> PN
mult = rho fMult gMult
fMult :: Vec -> PN
fMult = z
gMult :: Vec -> PN
gMult = add `circ` [pr 3, pr 2]

-- 8) Significance
sig :: PN -> Bool
sig ['0'] = False
sig _ = True

-- 9) Equivalence
eq :: Vec -> PN
eq [x:xrest, y:yrest] =
  if x == y
    then eq [xrest, yrest]
    else "S0"

-- 10a) Less Than
lt :: PN -> PN -> PN
lt "0" "0" = "0"
lt "0" (_:_) = "S0"
lt (_:_) "0" = "0"
lt ('S':xs) ('S':ys) = lt xs ys

-- 10b) Division
divd :: Vec -> PN
divd [n, x] = divHelper n x
  where
    divHelper :: PN -> PN -> PN
    divHelper n x = if lt x n == "S0"
      then "0"
      else s [divHelper n (tsub [n, x])]

-- 11) Exponentiation
expo :: Vec -> PN
expo = rho fExpo gExpo
fExpo :: Vec -> PN
fExpo _ = "S0"
gExpo :: Vec -> PN
gExpo = mult `circ` [pr 3, pr 2]
