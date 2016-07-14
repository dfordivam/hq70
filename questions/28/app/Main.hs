module Main where

import qualified Data.Vector as V
import Data.List (sortBy)

type StudentCount = Int
type Area = Int

tableData :: V.Vector (Area, StudentCount)
tableData = V.fromList [(11000, 40)
  ,(8000,30)
  ,(400,24)
  ,(800,20)
  ,(900,14)
  ,(1800,16)
  ,(1000,15)
  ,(7000,40)
  ,(100,10)
  ,(300,12)]

numOfStudents :: StudentCount
numOfStudents = 150

computeAnswer :: (Area, StudentCount)
computeAnswer = answer
 where
  -- Take ratios,
  ratios :: V.Vector Double
  ratios = V.map (\(x,y) -> (fromIntegral x) / (fromIntegral y)) tableData 

  ratiosWithIndex :: V.Vector (Int, Double)
  ratiosWithIndex = V.indexed ratios

  -- sort on ratios, The biggest (Area/StudentCount) comes first
  -- This returns the index of vector
  sorted :: [Int]
  sorted = map (\(i,_) -> i) $ reverse $ sortBy mySort (V.toList ratiosWithIndex)
    where mySort a1@(_,x1) a2@(_,x2) = compare x1 x2

  -- get the studentcount and list of indices for all the cumulative indices
  cumStudents :: [(StudentCount, [Int])]
  cumStudents = map (\x -> (snd (addAll x),x)) $ cumIndex

  -- take the entry which is just less than 150, 
  -- and get its total (area, studentcount) 
  answer = addAll $ snd $ last $ takeWhile (\(x,idx) -> x < numOfStudents) cumStudents

  -- Some useful code
  --
  -- Add the area and student count for the given list of indices
  addAll :: [Int] -> (Area, StudentCount)
  addAll idx = foldl (\(a1, s1) i -> (a1 + (fst (tableData V.! i)), s1 + (snd (tableData V.! i)))) (0, 0) idx

  -- Cumulative indices
  -- Create a list of all the cumulative indices
  cumIndex :: [[Int]]
  cumIndex = loop (length sorted)
    where
      loop :: Int -> [[Int]]
      loop 0 = []
      loop n = loop (n-1) ++ [(take n sorted)]

main = do
  let 答え = computeAnswer
  putStrLn $ "For 150 人 the maximum area is " ++ (show 答え)
