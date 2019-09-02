isRound x = x `mod` 10 == 0
hasLastDigit x y = x `mod` 10 == y
isHalfRound x = x `hasLastDigit` 0 || x `hasLastDigit` 5
x `divides` y = x `div` y == 0
twice f x = f (f x)
countCountDigits x = twice countDigits x
sumSumDigits x = twice sumDigits x
sumDigitsWith f n = if n < 10 then f n else sumDigitsWith f (n `div` 10) + f (n `mod` 10)
countDigits = sumDigitsWith (\d -> 1)
sumDigits = sumDigitsWith (\d -> d)
fixEq f x = if x == f x then x else fixEq f (f x)
isMultipleOf3 x = fixEq sumDigits x == 3 || fixEq sumDigits x == 6 || fixEq sumDigits x == 9

