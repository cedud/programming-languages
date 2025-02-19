import Data.List

-- Task 1
pXX :: Int -> Int -> Bool
pXX m x = (fromIntegral x / fromIntegral m) * (fromIntegral (x - 1) / fromIntegral (m - 1)) == 0.5

smallestM :: Int -> Int
smallestM n = head [m | m <- [n..], satisfies m]
  where
    satisfies m = any (\x -> x >= (m-x) && pXX m x == True) [m `div` 2 .. m] -- Haskell Lambda

-- Task 2
combinations :: [Int] -> Int -> [[Int]] -- Number of subsets
combinations _ 0 = [[]]
combinations [] _ = [] 
combinations (x:xs) k = 
    let withX = map (x:) (combinations xs (k-1))
        withoutX = combinations xs k   
    in withX ++ withoutX

uniqueSubsetSums :: [Int] -> Int -> Int -- Sum of unique subsets
uniqueSubsetSums t k = 
    let 
        subsets = combinations t k
        sums = sort $ map sum subsets
        sumCounts = map length . group $ sums
        uniqueSums = [sum | (sum, count) <- zip (nub sums) sumCounts, count == 1]
    in sum uniqueSums

-- Task 3
isPalindrome :: Int -> Bool
isPalindrome x = show x == reverse (show x)

largestPalindrome :: Int -> Int
largestPalindrome n = maximum [x*y | x <- range, y <- range, isPalindrome(x*y) ] 
    where 
    range = [10^(n-1) .. 10^n - 1]

-- Main
main :: IO ()
main = do 
    print (largestPalindrome 2)
    print (uniqueSubsetSums [1,3,6,8,10,11] 3)
    print (smallestM 20)