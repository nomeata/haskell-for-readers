data Suit = Diamonds | Clubs | Hearts | Spades

isRed :: Suit -> Bool
isRed s = case s of
  Diamonds -> True
  Hearts -> True
  _ -> False

