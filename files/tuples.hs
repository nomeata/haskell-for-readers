data TwoIntegers = TwoIntegers Integer Integer
splitLastDigit :: Integer -> TwoIntegers
splitLastDigit n = TwoIntegers (n `div` 10) (n `mod` 10)

