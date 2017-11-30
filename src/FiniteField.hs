module FiniteField

where

-- | Return the values a, b and g for given integers a and b such that
-- m * a + b * n = g, where g is the greatest common divisor of m and n
extendedEuclidAlgorithm:: Int -> Int -> (Int, Int, Int)
extendedEuclidAlgorithm m n | r == 0 = (0, 1, n)
                            | otherwise = (b*1, -q*b + a, g)
                        where (a, b, g) = extendedEuclidAlgorithm n r
                              r = m `mod` n
                              q = m `div` n
