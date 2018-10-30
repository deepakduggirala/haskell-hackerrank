import Data.List
primes =
  [ 2
  , 3
  , 5
  , 7
  , 11
  , 13
  , 17
  , 19
  , 23
  , 29
  , 31
  , 37
  , 41
  , 43
  , 47
  , 53
  , 59
  , 61
  , 67
  , 71
  , 73
  , 79
  , 83
  , 89
  , 97
  , 101
  , 103
  , 107
  , 109
  , 113
  , 127
  , 131
  , 137
  , 139
  , 149
  , 151
  , 157
  , 163
  , 167
  , 173
  , 179
  , 181
  , 191
  , 193
  , 197
  , 199
  , 211
  , 223
  , 227
  , 229
  , 233
  , 239
  , 241
  , 251
  , 257
  , 263
  , 269
  , 271
  , 277
  , 281
  , 283
  , 293
  , 307
  , 311
  , 313
  , 317
  , 331
  , 337
  , 347
  , 349
  , 353
  , 359
  , 367
  , 373
  , 379
  , 383
  , 389
  , 397
  , 401
  , 409
  , 419
  , 421
  , 431
  , 433
  , 439
  , 443
  , 449
  , 457
  , 461
  , 463
  , 467
  , 479
  , 487
  , 491
  , 499
  , 503
  , 509
  , 521
  , 523
  , 541
  , 547
  , 557
  , 563
  , 569
  , 571
  , 577
  , 587
  , 593
  , 599
  , 601
  , 607
  , 613
  , 617
  , 619
  , 631
  , 641
  , 643
  , 647
  , 653
  , 659
  , 661
  , 673
  , 677
  , 683
  , 691
  , 701
  , 709
  , 719
  , 727
  , 733
  , 739
  , 743
  , 751
  , 757
  , 761
  , 769
  , 773
  , 787
  , 797
  , 809
  , 811
  , 821
  , 823
  , 827
  , 829
  , 839
  , 853
  , 857
  , 859
  , 863
  , 877
  , 881
  , 883
  , 887
  , 907
  , 911
  , 919
  , 929
  , 937
  , 941
  , 947
  , 953
  , 967
  , 971
  , 977
  , 983
  , 991
  , 997
  ]

main :: IO ()
main = interact $ unlines . map solve . tail . lines


solve :: String -> String
solve n | '0' `elem` n = "DEAD"
        | otherwise    = deck (all isPrime . left $ n) (all isPrime . right $ n)

deck True  True  = "CENTRAL"
deck True  False = "LEFT"
deck False True  = "RIGHT"
deck False False = "DEAD"

isPrime :: Int -> Bool
isPrime 1 = False
isPrime k = null [ x | x <- filter (<= isqrt k) primes, k `mod` x == 0 ]

left :: String -> [Int]
left = reverse . map read . filter (not . null) . tails

right :: String -> [Int]
right = map read . filter (not . null) . inits

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral