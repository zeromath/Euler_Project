import Data.List

factors :: Int -> Int
factors 1 = 1
factors n = 1 + sum list_of_small_factors + (sum . map (n `div`) $ list_of_small_factors) - extra_factor
  where
    m = round(sqrt(fromIntegral(n)))
    list_of_small_factors = filter (\x -> n `mod` x == 0) [2..m]
    extra_factor
      | m^2 == n  = m
      | otherwise = 0

abundant_number_list = filter (\x -> x < factors x) . take 28123 $ [1..]

notSum :: Int -> Bool
notSum n = (map (`subtract` n) l) `intersect` l == []
  where
    l = takeWhile (<n) abundant_number_list

main :: IO()
main = do
 let x = sum . filter notSum $ [1..28123]
 print x -- 4179871
