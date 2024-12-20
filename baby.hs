doubleMe x = x + x

doubleUs x = doubleMe x + doubleMe x

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

length' xs = sum [1 | _ <- xs]

asymptotic' :: (Integral a) => a -> a
asymptotic' a = exponential
  where
    exponential = 2 ^ a
