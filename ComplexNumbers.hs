module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Com a a deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry Com

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Com r i) = Com r (i * (-1))

abs :: Floating a => Complex a -> a
abs (Com r i) = sqrt (r*r + i*i)

real :: Num a => Complex a -> a
real (Com r _) = r

imaginary :: Num a => Complex a -> a
imaginary (Com _ i) = i

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Com r i) (Com r' i') = Com
  (r * r' - i * i')
  (r * i' + i * r')

add :: Num a => Complex a -> Complex a -> Complex a
add (Com r i) (Com r' i') = Com (r + r') (i + i')

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Com r i) (Com r' i') = Com (r - r') (i - i')

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Com r i) (Com r' i') = Com
  (( r * r' + i * i' ) / denom)
  (( r' * i - r * i' ) / denom)
  where
    denom = i'^2 + r'^2
