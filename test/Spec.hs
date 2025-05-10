import Lib

main :: IO ()
main = do
    print $ (diffcoef (\x -> 2 * x) 1 :: Float) -- 2
    print $ (diffcoef (\x -> x * x) 3 :: Float) -- 6
    print $ (jacob (\(r, th) -> (r * cos th, r * sin th)) (1, pi/3) :: [(Float, Float)]) -- [(1/2, sqrt(3)/2), (-sqrt(3)/2, 1/2)]
