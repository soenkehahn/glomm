

module Test.T11 (main) where


main :: String
main = show $ map (+ 3) (take 10 [0 .. 50])
