module EllipticCurve

where

import Data.Bits

-- | Return the values a, b and g for given integers a and b such that
-- m * a + b * n = g, where g is the greatest common divisor of m and n
extendedEuclidAlgorithm:: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclidAlgorithm m n | r == 0 = (0, 1, n)
                            | otherwise = (b, -q*b + a, g)
                        where (a, b, g) = extendedEuclidAlgorithm n r
                              r = m `mod` n
                              q = m `div` n

data Curve = Curve {
    a :: Integer,
    b :: Integer,
    modulus :: Integer -- a prime
} deriving (Show)

data Point = Point {
    x :: Integer,
    y :: Integer,
    curve :: Curve,
    isInfinity :: Bool
} deriving(Show, Eq)

instance Eq Curve where
    (==) c1@(Curve a1 b1 m1) c2@(Curve a2 b2 m2) = (a1 == a2) && (b1 == b2) && (m1 == m2)

instance Num Point where
    p1 + p2
        | isInfinity p1 = p2
        | isInfinity p2 = p1
        | p1 == p2 = _double p1
        | otherwise = Point x' y' c False
        where x1 = x p1
              y1 = y p1
              x2 = x p2
              y2 = y p2
              c = curve p1
              x' = (slope ^ 2 - x1 - x2) `mod` prime
              y' = (slope * (x1 - x') - y1) `mod` prime
              slope = ((y1-y2) * inv_dx ) `mod` prime
              (inv_dx, _, _) = extendedEuclidAlgorithm (x1-x2) prime
              prime = modulus c

_double :: Point -> Point
_double p1@(Point x1 y1 c inf)
            | inf = p1
            | otherwise = Point x' y' c False
            where x' = (slope ^ 2 - 2* x1) `mod` prime
                  y' = (slope * (x1 - x') - y1) `mod` prime
                  slope = ((3 * x1^2 + a c) * inv_dx ) `mod` prime
                  (inv_dx, _, _) = extendedEuclidAlgorithm (2*y1) prime
                  prime = modulus c


scalePoint :: Point -> Integer -> Point
scalePoint p = _scale (Point 0 0 (curve p) True) p
    where _scale sm p 0 = sm
          _scale sm p n | n .&. 1 == 1 = _scale (sm + p2) p2 (shiftR n 1)
            where p2 = _double p

