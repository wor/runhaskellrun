import qualified Data.List as L
import Control.Monad (replicateM)
-- import Data.AEq ((~==))

--
-- group edges are not the only place where, moving can be beneficial, the
-- distance (cost) matters
-- We have to consider only the paths from size of 1 to the size of n,
-- which has the global maximum.

-- So the question is to, how to calculate all possible paths.
-- path_up_cost - path_down_cost > 0 = True =>
-- 5->4     4<-3, 3<-2, 2<-1
--  1        1      -1   10
--
-- So first just modify initial set allocation to be ordered by discount
-- ammount of the set size.
-- Next Modify set size modification to find 

-- What is the optimum, and how to determine the optimum discount in this
-- case.
-- First start with largest ss-dist (set size distance)
-- * max set size 5 in this example
-- * the largest 
-- * Is there possibility to move above the other (cross)?
-- largest change is 1 ( (floor(c)+1)*2 = max set size)
-- => floor((mss/2)-1) = c
--
-- Is this enough, how to permutate all the options.
-- Find the largest and the smallest distances of same size and try to move
--  -> What is the quilibrium or optimum situation in this, how it is
--  detected?
--
-- 5->4 = ddist_1
--  1->2 = ddist_1_low_1
--  2->3 = ddist_1_low_2
-- ==== if ddist_1_low_* - ddist_1 >0 make a move and start again from
-- ==== beginning
-- 4->3 = ddist_2
--  1->2 = ddist_2_low_1
-- ==== if ddist_1_low_* - ddist_1 >0 make a move and start again from
-- ==== beginning

-- We really have to make checks for larger length of routes than just 1, as the
-- first step might be negative but second could be more than enough positive to
-- make up for it.

-- * max set size 11 in this example
-- First routes of length 1, then routes of length 2 and so on to largest
-- change value which with 11 is 4
-- 11->10
--  1->2
--  2->3
--  3->4
--  4->5
--  5->6
--  6->7
--  7->8
--  8->9
-- 10->9
--  1->2
--  2->3
--  ...
--  7->8

--
--
-- 5->4->3   <-3<-2


main :: IO ()
main = do
          -- let l = [0.0, 0.05, 0.10, 0.20, 0.25]
          -- let l = [0.0, 0.05, 0.1, 0.15, 0.21, 0.30, 0.05, 0.14]
          let l = [0.0, 0.50, 0.45, 0.75, 0.76, 0.77, 0.78, 0.79, 0.78, 0.1] :: [Double]
          -- improvement, when moving upward, the first index though corresponds
          -- to staying at the same position.
          -- [1->1, 1->2, 2->3, 3->4, ... ,len-1->len]
          let up_distances = 0:(zipWith subtract l $ tail l)
          print "Length of l:"
          print $ length l
          -- This is permutations(range(1,11), 2) in python with repeated
          -- elements (1,1),(2,2), and so on..
          let comb = replicateM 2 [1..length l]
          print "Length of comb:"
          print $ length comb
          print "--- combinations of 2 of l ----"
          print comb
          print "--- ---------------------- ----"

          -- Now we have the possible combinations of single movement which is
          -- pair of positions.

          -- Next we need is a pair of these movements where the other is
          -- upwards and the other is downwards.

          -- All possible upwards and downwards moves
          -- Meaning all route lengths.
          let upwards = filter (\[x,y] -> x < y) comb
          let downwards = filter (\[x,y] -> x > y) comb

          print upwards
          print downwards

          -- Now calculate distances of the pairs and combine pairs which where
          -- improvement can be made
          --
          -- Execute improvements

          -- Could we use sorting, to select pairs causing improvement
          -- (up) - (low) > 0

          -- let upwards_dist = [ x | x <- upwards, 

          -- Proceed to route lenght 2 and so on to the max route length
          -- TODO: here

          let l1 = 0:(zipWith subtract l $ tail l)
          putStrLn $ "l1: " ++ show l1
          print $ length l1

          -- let l1 = zipWith subtract l $ tail l
          -- putStrLn $ "l1: " ++ show l1
          -- print $ length l1

          -- let l2 = zip l1 [1..length l1]
          -- putStrLn $ "l2: " ++ show l2
          -- print $ length l2
          -- -- Does sort work the same way for tuples?
          -- let l3 = L.sortBy (\(x,_) (y,_) -> almostEQCompare x y) l2
          -- print l3
          -- -- Next try grouping
          -- let g1 = L.groupBy (\(x,_) (y,_) -> grouper x y) l3
          -- print "------------"
          -- printGroups g1
          -- print g1
          -- let g2 = groupClean g1
          -- print "------------ cleaned groups"
          -- print g2
          -- printGroups g2
          -- print "------------"
          -- Now we have tuple of discount ammount and list of items which
          -- belong to that discount ammount group.
          -- Every item can belong to maximum two discount groups!
          --

calculateRouteDistance                        :: [Double] -> Int -> Int -> Double
calculateRouteDistance values startInd endInd = values!!endInd - values!!startInd



-- almostEQCompare :: Double -> Double -> Ordering
-- almostEQCompare x y
--                 | x ~== y = EQ
--                 | x > y   = GT
--                 | x < y   = LT
-- 
-- grouper :: Double -> Double -> Bool
-- grouper x y = abs x ~== abs y
-- 
-- 
-- printGroups :: (Show a) => [a] -> IO ()
-- printGroups []     = print "-------------"
-- printGroups (x:xs) = do putStr "Group: "
--                         print x
--                         printGroups xs
-- 
-- groupClean :: [[(Double, Int)]] -> [(Double, [Int])]
-- groupClean []     = []
-- groupClean (x:xs) = (remDub.foldToTuple) x : groupClean xs
--                     where
--                           foldToTuple = foldr (\(j,k) (_,l) -> (j, k:(k+1):l)) (0,[])
--                           remDub (y, l) = (y, L.nub l)
