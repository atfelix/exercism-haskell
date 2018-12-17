module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 exp,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as P

-- Data definition -------------------------------------------------------------
data Complex a = Complex { real :: a, imaginary :: a } deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex (re, im) = Complex re im

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate z = complex (real z, negate (imaginary z))

abs :: Floating a => Complex a -> a
abs z = sqrt (real z * real z + imaginary z * imaginary z)

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex a b) (Complex c d) = complex (a * c - d * b, a * d + b * c)

add :: Num a => Complex a -> Complex a -> Complex a
add z w = complex (real z + real w, imaginary z + imaginary w)

sub :: Num a => Complex a -> Complex a -> Complex a
sub z w = z `add` (complex (-1, 0) `mul` w)

div :: Fractional a => Complex a -> Complex a -> Complex a
z `div` w = (z `mul` conjugate w) `mul` complex (1 / denom, 0)
    where denom = real w * real w + imaginary w * imaginary w

exp :: Floating a => Complex a -> Complex a
exp (Complex re im) = Complex (e re * cos im) (e re * sin im)
    where e = P.exp
