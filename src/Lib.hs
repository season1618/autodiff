{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Lib(
    diff, diffcoef, jacob
) where

import Data.Kind

data D a = D a a

instance Num a => Num (D a) where
    negate (D x dx) = D (- x) (- dx)
    (D x dx) + (D y dy) = D (x + y) (dx + dy)
    (D x dx) * (D y dy) = D (x * y) (dx * y + x * dy)
    abs (D x dx) = D (abs x) dx
    signum (D x dx) = D (signum x) dx
    fromInteger i = D (fromInteger i) (fromInteger 0)

instance Fractional a => Fractional (D a) where
    (D x dx) / (D y dy) = D (x / y) ((dx * y - x * dy) / (y * y))
    fromRational r = D (fromRational r) (fromRational 0)

instance Floating a => Floating (D a) where
    pi = D pi 0
    exp (D x dx) = D (exp x) (exp x * dx)
    log (D x dx) = D (log x) (dx / x)
    sin (D x dx) = D (sin x) (cos x * dx)
    cos (D x dx) = D (cos x) (-sin x * dx)
    tan (D x dx) = D (tan x) (dx / (cos x * cos x))
    asin (D x dx) = D (asin x) (dx / sqrt (1 - x * x))
    acos (D x dx) = D (acos x) (- dx / sqrt (1 - x * x))
    atan (D x dx) = D (atan x) (dx / (1 + x * x))
    sinh (D x dx) = D (sinh x) (cosh x * dx)
    cosh (D x dx) = D (cosh x) (sinh x * dx)
    tanh (D x dx) = D (tanh x) (dx / (cosh x * cosh x))
    asinh (D x dx) = D (asinh x) (dx / sqrt (x * x + 1))
    acosh (D x dx) = D (acosh x) (dx / sqrt (x * x - 1))
    atanh (D x dx) = D (atanh x) (dx / (1 - x * x))

type family Dual (a :: Type) :: Type where
    Dual (a, b) = (D a, D b)
    Dual (a, b, c) = (D a, D b, D c)
    Dual (a, b, c, d) = (D a, D b, D c, D d)
    Dual a = D a

class EqDual a where
    base :: [a]
    toDual :: D a -> Dual a
    fromDual :: Dual a -> D a

instance EqDual Float where
    base = [1]
    toDual (D x dx) = D x dx
    fromDual (D x dx) = D x dx

instance Num a => EqDual (a, a) where
    base = [(1, 0), (0, 1)]
    toDual (D (x0, x1) (dx0, dx1)) = (D x0 dx0, D x1 dx1)
    fromDual (D x0 dx0, D x1 dx1) = D (x0, x1) (dx0, dx1)

instance Num a => EqDual (a, a, a) where
    base = [(1, 0, 0), (0, 1, 0), (0, 0, 1)]
    toDual (D (x0, x1, x2) (dx0, dx1, dx2)) = (D x0 dx0, D x1 dx1, D x2 dx2)
    fromDual (D x0 dx0, D x1 dx1, D x2 dx2) = D (x0, x1, x2) (dx0, dx1, dx2)

instance Num a => EqDual (a, a, a, a) where
    base = [(1, 0, 0, 0), (0, 1, 0, 0), (0, 0, 1, 0), (0, 0, 0, 1)]
    toDual (D (x0, x1, x2, x3) (dx0, dx1, dx2, dx3)) = (D x0 dx0, D x1 dx1, D x2 dx2, D x3 dx3)
    fromDual (D x0 dx0, D x1 dx1, D x2 dx2, D x3 dx3) = D (x0, x1, x2, x3) (dx0, dx1, dx2, dx3)

diff :: (EqDual a, EqDual b) => (Dual a -> Dual b) -> a -> (a -> b)
diff f x = \dx ->
    let (D _ dy) = fromDual . f . toDual $ (D x dx)
    in dy

diffcoef :: (EqDual a, Fractional a) => (Dual a -> Dual a) -> (a -> a)
diffcoef f = \x -> diff f x 1

-- (y1, y2, ...) = f (x1, x2, ...)
-- Jf = [(dy1/dx1, dy2/dx1, ...), (dy1/dx2, dy2/dx2, ...), ...]
jacob :: (EqDual a, EqDual b) => (Dual a -> Dual b) -> a -> [b]
jacob f x = fmap (diff f x) base

