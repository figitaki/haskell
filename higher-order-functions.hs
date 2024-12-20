multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithOneHundred :: (Num a, Ord a) => a -> Ordering
compareWithOneHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x : xs) =
  let smallerSorted = quicksort' (filter (<= x) xs)
      biggerSorted = quicksort' (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

largestDivisble :: (Integral a) => a
largestDivisble = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)
