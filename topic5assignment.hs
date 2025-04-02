main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd numbers: " ++ show (filter odd [10..20]))
  putStrLn ("Sum: " ++ show (add 5 4))

add :: Integer -> Integer -> Integer
add x y = x + y
