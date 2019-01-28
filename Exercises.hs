-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal	
splitSort :: Ord a => [a] -> [[a]] 
splitSort [] = []
splitSort ns 
    | length ns > 1 = [take i ns] ++ splitSort (drop i ns)
    | otherwise = [ns]
    where
        i
            | ns !! 0 < ns !! 1  = getIndex ns 0
            | ns !! 0 > ns !! 1  = getIndex ns 1
            | ns !! 0 == ns !! 1 = getIndex ns 2

getIndex :: Ord a => [a] -> Int -> Int
getIndex ns x
    | tail ns == [] = 1
    | x == 0 && ns !! 0 < ns !! 1  = 1 + getIndex (tail ns) x
    | x == 1 && ns !! 0 > ns !! 1  = 1 + getIndex (tail ns) x  
    | x == 2 && ns !! 0 == ns !! 1 = 1 + getIndex (tail ns) x
    | otherwise                    = 1

-- Exercise 2
-- longest common sub-list of a finite list of finite list
getPowerSet [] = [[]]
getPowerSet (x:xs) = [x:sublist | sublist <- getPowerSet xs] ++ getPowerSet xs

count x xs = (length . filter (== x)) xs

longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList xs =
    foldl1 (\x y -> if length x > length y then x else y) intersections
    where powersets = foldr (++) [] (map getPowerSet xs)
          intersections = filter (\x -> count x powersets == length xs) powersets

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show

canProgress :: [ModuleResult] -> Bool
canProgress ms 
    | sumCredits ms >= 60 = True
    | otherwise = False 

sumCredits :: [ModuleResult] -> Float
sumCredits [] = 0
sumCredits ms
    | length ms > 0 && getModuleMarks (head ms) > 40                                  = getModuleCredits (head ms) + sumCredits (tail ms)
    | length ms > 0 && getModuleMarks (head ms) < 40 && getModuleMarks (head ms) > 25 = ((getModuleMarks (head ms)) - 25) + sumCredits (tail ms)
    | otherwise = 0

getModuleCredits :: ModuleResult -> Float
getModuleCredits (ModuleResult c _) = c

getModuleMarks :: ModuleResult -> Float
getModuleMarks (ModuleResult _ m) = fromIntegral m

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms
    | finalScore >= 70 && upgradeScore ms First == False = First
    | finalScore >= 68 && upgradeScore ms First == True = First
    | 70 > finalScore && finalScore >= 60 && upgradeScore ms UpperSecond == False = UpperSecond
    | 60 > finalScore && finalScore >= 58 && upgradeScore ms UpperSecond == True = UpperSecond
    | 60 > finalScore && finalScore >= 50 && upgradeScore ms LowerSecond == False = LowerSecond
    | 50 > finalScore && finalScore >= 48 && upgradeScore ms LowerSecond == True = LowerSecond
    | otherwise = Third
    where finalScore = totalWeightedMark ms / totalWeightedCredits ms

totalWeightedCredits :: [[ModuleResult]] -> Float
totalWeightedCredits ms 
    | length ms == 3 = sum (map getModuleCredits (ms !! 1)) + sum (map getModuleCredits (ms !! 2)) * 2
    | length ms == 4 = sum (map getModuleCredits (ms !! 1)) + sum (map getModuleCredits (ms !! 2)) * 2 + sum (map getModuleCredits (ms !! 3)) * 2
    | otherwise = 1

totalWeightedMark :: [[ModuleResult]] -> Float
totalWeightedMark ms 
    | length ms == 3 = sum (zipWith (*) (map getModuleCredits (ms !! 1)) (map getModuleMarks (ms !! 1))) + sum (zipWith (*) (map getModuleCredits (ms !! 2)) (map getModuleMarks (ms !! 2))) * 2
    | length ms == 4 = sum (zipWith (*) (map getModuleCredits (ms !! 1)) (map getModuleMarks (ms !! 1))) + sum (zipWith (*) (map getModuleCredits (ms !! 2)) (map getModuleMarks (ms !! 2))) * 2 + sum (zipWith (*) (map getModuleCredits (ms !! 3)) (map getModuleMarks (ms !! 3))) * 2
    | otherwise = 1


upgradeScore :: [[ModuleResult]] -> DegreeClass -> Bool
upgradeScore mss d
    | d == First && markPercentage >= 0.7 = True
    | d == UpperSecond && markPercentage >= 0.6 = True
    | d == LowerSecond && markPercentage >= 0.5 = True
    | otherwise = False
    where markPercentage = (bandedMark mss d) / (totalMarks mss)

bandedMark :: [[ModuleResult]] -> DegreeClass -> Float
bandedMark mss d
    | length mss == 3 = filterBand (map getModuleMarks (mss !! 1)) + filterBand (map getModuleMarks (mss !! 2))
    | length mss == 4 = filterBand (map getModuleMarks (mss !! 1)) + filterBand (map getModuleMarks (mss !! 2)) + filterBand (map getModuleMarks (mss !! 3))
    | otherwise = 0
    where filterBand xs
            | d == First = sum (filter (>=70) xs)
            | d == UpperSecond = sum (filter (>=60) xs)
            | d == LowerSecond = sum (filter (>=50) xs)

totalMarks :: [[ModuleResult]] -> Float
totalMarks mss
    | length mss == 3 = sum (map getModuleMarks (mss !! 1)) + sum (map getModuleMarks (mss !! 2))
    | length mss == 4 = sum (map getModuleMarks (mss !! 1)) + sum (map getModuleMarks (mss !! 2)) + sum (map getModuleMarks (mss !! 3))
    | otherwise = 1
    
-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
testFunc :: Float -> Float
testFunc x = (2-x)*(x+1)
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps 
    | abs ((d x) - (d x')) <= eps = (x + x') / 2
    | otherwise = hillClimb d z zp eps
    where z = findX d x x'
          zp = findXP d x x'

findX :: (Float -> Float) -> Float -> Float -> Float
findX d x x' 
    | d (x) > d (x') = x
    | otherwise = x' - 0.618 * (x' - x)

findXP :: (Float -> Float) -> Float -> Float -> Float
findXP d x x'
    | d (x) < d (x') = x'
    | otherwise = x + 0.618 * (x' - x)

-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps
    | abs ((searchQuadratic xs x)^ 2 - (searchQuadratic xs x')^2) <= eps = (x + x') / 2
    | otherwise = nearestRoot xs z zp eps
    where z = findQuadraticX xs x x'
          zp = findQuadraticXP xs x x'

findQuadraticX :: [Float] -> Float -> Float -> Float
findQuadraticX xs x x' 
    | (searchQuadratic xs x)^ 2 > (searchQuadratic xs x')^ 2 = x' - 0.618 * (x' - x)
    | otherwise = x

findQuadraticXP :: [Float] -> Float -> Float -> Float
findQuadraticXP xs x x'
    | (searchQuadratic xs x)^ 2 < (searchQuadratic xs x')^ 2 = x + 0.618 * (x' - x)
    | otherwise = x'

searchQuadratic :: [Float] -> Float -> Float
searchQuadratic ns x
    | length ns > 0 = (ns !! ((length ns) - 1) * (x ^ ((length ns) - 1))) + searchQuadratic (init ns) x
    | otherwise = 0

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns [] = ns
executeInstructionSequence [] _ = []
executeInstructionSequence ns ins
    | length ns > 1 && head (ins) == Add      = executeInstructionSequence ([(ns !! 0) + (ns !! 1)] ++ drop 2 ns) (tail ins)
    | length ns > 1 && head (ins) == Subtract = executeInstructionSequence ([(ns !! 0) - (ns !! 1)] ++ drop 2 ns) (tail ins)
    | length ns > 1 && head (ins) == Multiply = executeInstructionSequence ([(ns !! 0) * (ns !! 1)] ++ drop 2 ns) (tail ins)
    | head (ins) == Duplicate = executeInstructionSequence ([ns !! 0] ++ ns) (tail ins)
    | head (ins) == Pop = executeInstructionSequence (drop 1 ns) (tail ins)
    | otherwise = ns

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence 1 = []
optimalSequence n = searchSpace [3] [3^n] []

findIndex n count (x:xs)
    | n == x = count
    | otherwise = findIndex n (count + 1) xs

checkQueue :: [Int] -> [[Instruction]] -> [Int] -> [Instruction]
checkQueue _ [] _ = []
checkQueue xs is ms
    | passed /= [] = is !! (findIndex (passed !! 0) 0 out)
    | otherwise = []
    where out = map (\x -> executeInstructionSequence xs x) is
          passed = filter (\x -> x == ms) out

getNewQueue :: [[Instruction]] -> [[Instruction]]
getNewQueue [] = [[Duplicate]]
getNewQueue qs = [x ++ [Multiply] | x <- qs] ++ [x ++ [Duplicate] | x <- qs]

searchSpace :: [Int] -> [Int] -> [[Instruction]] -> [Instruction]
searchSpace xs gs qs
    | ins /= [] = ins
    | otherwise = searchSpace xs gs newQueue
    where ins = checkQueue xs qs gs
          newQueue = getNewQueue qs

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = getUniqueElements (getMaxInstructionSet ns (getMaxInstruction ns (permutate (length ns - 1))) (permutate (length ns - 1)))

permutate :: Int -> [[Instruction]]
permutate x = [1..x] >>= (\n -> mapM (const [Add, Multiply, Pop]) [1..x])
getUniqueElements :: [[Instruction]] -> [[Instruction]]
getUniqueElements [] = []
getUniqueElements xs = (head xs) : getUniqueElements (filter (/= (head xs)) (tail xs))

getMaxInstruction :: [Int] -> [[Instruction]] -> [Instruction]
getMaxInstruction _ [] = []
getMaxInstruction zs xs 
    | length xs > 1 && (executeInstructionSequence zs (head xs)) <= (executeInstructionSequence zs (head (tail xs))) = getMaxInstruction zs (tail xs)
    | length xs > 1 && (executeInstructionSequence zs (head xs)) > (executeInstructionSequence zs (head (tail xs))) = getMaxInstruction zs ([(head xs)] ++ (drop 2 xs))
    | otherwise = head xs

getMaxInstructionSet :: [Int] -> [Instruction] -> [[Instruction]] -> [[Instruction]]
getMaxInstructionSet n x [] = []
getMaxInstructionSet n x xs
    | executeInstructionSequence n x == executeInstructionSequence n (head xs) = [(head xs)] ++ getMaxInstructionSet n x (tail xs)
    | otherwise = getMaxInstructionSet n x (tail xs)

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
 
getVertex :: Rectangle -> [(Int, Int)]
getVertex (Rectangle l r) = [(fst l, snd r), r, (fst r, snd l), l]
 
getAllPoints :: Rectangle -> [(Int, Int)]
getAllPoints rect = [(x, y) | x <- (map (+(fst (vertexes!!3))) xRange), y <- (map (+(snd (vertexes!!3))) yRange)]
  where
        vertexes = getVertex rect
        xDiff = (fst (vertexes!!1)) - (fst (vertexes!!3))
        yDiff = (snd (vertexes!!1)) - (snd (vertexes!!3))
        xRange = [0..xDiff]
        yRange = [0..yDiff]
 
noDup :: Eq a => [a] -> [a]
noDup = helperFunc []
    where helperFunc seen [] = seen
          helperFunc seen (x:xs)
              | x `elem` seen = helperFunc seen xs
              | otherwise = helperFunc (seen ++ [x]) xs
 
compareArea :: (Int -> Int -> Bool) -> Rectangle -> Rectangle -> Bool
compareArea f r1 r2 = f r1Area r2Area
    where r1Vertexes = getVertex r1
          r2Vertexes = getVertex r2
          r1Area = ((fst (r1Vertexes!!1)) - (fst (r1Vertexes!!3))) * ((snd (r1Vertexes!!1)) - (snd (r1Vertexes!!3)))
          r2Area = ((fst (r2Vertexes!!1)) - (fst (r2Vertexes!!3))) * ((snd (r2Vertexes!!1)) - (snd (r2Vertexes!!3)))
 
sort :: [Rectangle] -> [Rectangle]
sort [] = []
sort (x:xs) = (sort more) ++ [x] ++ (sort less)
    where vertexes = getVertex x
          less = filter (\r -> compareArea (<) r x) xs
          more = filter (\r -> compareArea (>=) r x) xs
 
noDupRects :: [Rectangle] -> [(Int, Int)] -> [Rectangle] -> [Rectangle]
noDupRects [] _ result = result
noDupRects (x:xs) seen result
    | fst (vertexes!!1) >= fst (vertexes!!3) &&
      snd (vertexes!!1) >= snd (vertexes!!3) &&
      (vertexes!!0 `notElem` seen ||
      vertexes!!1 `notElem` seen ||
      vertexes!!2 `notElem` seen ||
      vertexes!!3 `notElem` seen) = noDupRects xs (noDup (seen ++ getAllPoints x)) (result ++ [x])
    | otherwise = noDupRects xs seen result
    where vertexes = getVertex x
 
getEndEdgeRange :: [(Int, Int)] -> [(Int, Int)] -> Int -> Int
getEndEdgeRange edge seen increment
    | length outOfBoundCoords /= 0 = increment - 1
    | otherwise = getEndEdgeRange edge seen (increment + 1)
    where nextStep = map (\x -> ((fst x + increment), snd x)) edge
          outOfBoundCoords = filter (\x -> x `notElem` seen) nextStep
 
combineRects :: [(Int, Int)] -> [Rectangle] -> [Rectangle]
combineRects remains rects
    | remains == [] = rects
    | otherwise = combineRects (filter (`notElem` rectCoveredPtsMinusBorder) remains) (rects ++ [rectCovered])
    where leftEdge = filter (\x -> fst x == (foldl1 (\x y -> if x < y then x else y) (map (\x -> fst x) remains))) remains
          rightEdge = map (\x -> (fst x + (getEndEdgeRange leftEdge remains 0), snd x)) leftEdge
          bl = foldl1 (\x y -> if snd x < snd y then x else y) leftEdge
          tr = foldl1 (\x y -> if snd x > snd y then x else y) rightEdge
          rectCovered = Rectangle bl tr
          rectCoveredPts = getAllPoints rectCovered
          topEdge = filter (\x -> snd x == snd tr) rectCoveredPts
          botEdge = filter (\x -> snd x == snd bl) rectCoveredPts
          remainMinusRect = filter (`notElem` rectCoveredPts) remains
          leftBorderPts = filter (\x -> (fst x - 1, snd x) `elem` remainMinusRect) leftEdge
          rightBorderPts = filter (\x -> (fst x + 1, snd x) `elem` remainMinusRect) rightEdge
          topBorderPts = filter (\x -> (fst x, snd x + 1) `elem` remainMinusRect) topEdge
          botBorderPts = filter (\x -> (fst x, snd x - 1) `elem` remainMinusRect) botEdge
          rectCoveredPtsMinusBorder = filter (`notElem` (leftBorderPts ++ rightBorderPts ++ topBorderPts ++ botBorderPts)) rectCoveredPts
 
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList [] = []
simplifyRectangleList xs
    | length nonOverlappedRects <= length combinedRects = nonOverlappedRects
    | otherwise = combinedRects
    where nonOverlappedRects = noDupRects (sort xs) [] []
          combinedRects = combineRects (noDup (concat (map getAllPoints nonOverlappedRects))) []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = simplifyRectangleList(combineRects (getPoints x y a b) [])

getPoints :: Float -> Float -> Float -> Float -> [(Int, Int)]
getPoints x y a b = filter (\(w, z) -> (((fromIntegral w - x) / a)^2 + ((fromIntegral z - y) / b)^2) <= 1.0) pointData
    where pointData =  [(w, z) | w <- [round(x - a)..round(x + a)], z <- [round(y - b)..round(y + b)]]

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = translateBinary (obtainValues s)

obtainValues :: String -> [Int]
obtainValues [] = []
obtainValues xs
    | take 1 xs == "0" = [0] ++ obtainValues (tail xs)
    | take 1 xs == "1" = [1] ++ obtainValues (tail xs)
    | otherwise = obtainValues (tail xs)

translateBinary :: [Int] -> String
translateBinary [] = []
translateBinary xs 
    | take 2 xs == [0,0] = "a" ++ translateBinary (drop 2 xs)
    | take 2 xs == [0,1] = "b" ++ translateBinary (drop 2 xs)
    | take 2 xs == [1,0] = "c" ++ translateBinary (drop 2 xs)
    | take 2 xs == [1,1] = "d" ++ translateBinary (drop 2 xs)
    | otherwise = translateBinary (drop 2 xs)

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- use Cantor's diagonal method 
-- the first element of the result is 1 more than the first element of the first stream
-- the second element of the result is 1 more than the second element of the first and second streams
-- and so on
differentStream :: [[Int]] -> [Int]
differentStream ss = diagStream ss 0

diagStream :: [[Int]] -> Int -> [Int]
diagStream ss x 
    | (ss !! x) !! x == 0 = [1] ++ diagStream ss (x + 1)
    | (ss !! x) !! x /= 0 = [0] ++ diagStream ss (x + 1)

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
getSqrt :: Int -> Int
getSqrt n
    | out == fromInteger (round(out)) = round(out)
    | otherwise = getSqrt (n-1)
    where out = sqrt (fromIntegral n)
 
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f x y
    where b = getSqrt n
          maxXVal = b * b + b
          x = if n < maxXVal then n - maxXVal else b
          y = if n <= maxXVal then b else b - (n - maxXVal)

-- Exercise 15
sumTree :: [Int] -> [Int] -> Int
sumTree (x:xs) ns
    | length(endOrNot) == 0 && xs == [] = sum(ns ++ unpaired)
    | length(endOrNot) == 0 = sumTree xs (ns ++ unpaired)
    | length(endOrNot) == 1 = sumTree (xs ++ endOrNot) (ns ++ unpaired)
    | otherwise = sumTree (xs ++ unpaired) (ns ++ unpaired)
    where unpaired = [fst(unpair x), snd(unpair x)]
          stopList = [0,1]
          endOrNot = filter (\x -> x `notElem` stopList) unpaired
 
unpair :: Int -> (Int, Int)
unpair n = (x, y)
    where b = getSqrt n
          maxXVal = b * b + b
          x = if n < maxXVal then n - maxXVal else b
          y = if n <= maxXVal then b else b - (n - maxXVal)

isShellTreeSum :: Int -> Bool
isShellTreeSum n
    | sumTree [fst(unpaired)] [] == snd(unpaired) = True
    | otherwise = False
    where unpaired = unpair n