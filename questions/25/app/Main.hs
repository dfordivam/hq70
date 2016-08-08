module Main where

import qualified Data.Vector as V
import Data.List (sortBy)
import qualified Data.Set as Set

type HoleNo = Int

type HoleSet = Set.Set HoleNo
type EmptyHoles = (HoleSet, HoleSet)

type Connection = (HoleNo, HoleNo)

-- Two connections intersect if l1 > l2 and r1 < r2 or l1 < l2 and r1 > r2
getNumberOfIntersections :: [Connection] -> Int
getNumberOfIntersections (c:cs) = getIntersections c cs
  where
    getIntersections :: Connection -> [Connection] -> Int
    getIntersections _ [] = 0
    getIntersections c conns@(c1:c1s) = countTrue (map (doesIntersect c) conns) + (getIntersections c1 c1s)
      where
        countTrue :: [Bool] -> Int
        countTrue b = length $ filter id b
        doesIntersect (l1,r1) (l2,r2) 
          | (l1 > l2) && (r1 < r2) = True
          | (l1 < l2) && (r1 > r2) = True
          | otherwise              = False
          
-- Get permutations
getAllIntersections :: Int -> [[Connection]]
getAllIntersections = undefined

-- Available left and right holes
getIntersectionForHoles :: [HoleNo] -> [HoleNo] -> [[Connection]]
getIntersectionForHoles left right =  undefined

topConnections :: Int -> ([Connection], [Connection])
topConnections holes = 
  ([(1,j) | j <- [2 .. holes]], [(j,1) | j <- [2 .. holes]])

-- for each pair of top connections as starting point
-- find all other connections
--
-- holes top_left top_right

getPermutationsTree :: EmptyHoles -> [[Connection]]
getPermutationsTree emptyHoles = 
  case length (fst emptyHoles) of
    0 -> []
    _ -> concat $ map moreLevel oneLevel
  where oneLevel = getOneConnection emptyHoles
        moreLevel :: (Connection, EmptyHoles) -> [[Connection]]
        moreLevel (c, eh) = map addThisConn (getPermutationsTree eh)
          where addThisConn lst = map (c:) lst
  

-- generate a connection and return the rest of empty holes
getOneConnection :: EmptyHoles -> [(Connection, EmptyHoles)]
getOneConnection (leftSet, rightSet) = concat $ map (map getConnection (Set.elems leftSet)) (Set.elems rightSet )
  where getConnection :: HoleNo -> HoleNo -> (Connection, EmptyHoles, EmptyHoles)
        getConnection l1 r1 = ((l1, r1), (Set.delete l1 leftSet, Set.delete r1 rightSet))

computeAnswer = 1

main = do
  let 答え = computeAnswer
  putStrLn $ "For 150 人 the maximum area is " ++ (show 答え)
