-- file Riemann.hs
module Riemann where
data Complex = C Integer Integer
data Riemann = Complex Complex | Infinity

eqComplex :: Complex -> Complex -> Bool
eqComplex (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

eqRiemann :: Riemann -> Riemann -> Bool
eqRiemann (Complex c1) (Complex c2) = c1 `eqComplex` c2
eqRiemann Infinity Infinity = True
eqRiemann _ _ = False

instance Eq Complex where
    C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2
instance Eq Riemann where
    (==) = eqRiemann

