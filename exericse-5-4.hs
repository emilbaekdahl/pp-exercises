cfrac :: Float -> Integer -> [Integer]

cfrac r 0 = []
cfrac r n = [int] ++ (cfrac (1 / frac) (n - 1))
  where int = floor r
        f = r - int

main = print $ cfrac 3.123 10
