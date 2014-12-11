import qualified Data.List as L
import Data.AEq ((~==))


main :: IO ()
main = do
          -- let l = [0.0, 0.05, 0.10, 0.20, 0.25]
          let l = [0.0, 0.05, 0.1, 0.15, 0.21, 0.30, 0.05, 0.14]
          print $ length l

          let l1 = zipWith subtract l $ tail l
          putStrLn $ "l1: " ++ show l1
          print $ length l1

          let l2 = zip l1 [1..length l1]
          putStrLn $ "l2: " ++ show l2
          print $ length l2
          -- Does sort work the same way for tuples?
          let l3 = L.sortBy (\(x,_) (y,_) -> almostEQCompare x y) l2
          print l3
          -- Next try grouping
          let g1 = L.groupBy (\(x,_) (y,_) -> grouper x y) l3
          print "------------"
          printGroups g1
          print g1
          let g2 = groupClean g1
          print "------------ cleaned groups"
          print g2
          printGroups g2
          print "------------"
          -- Now we have tuple of discount ammount and list of items which
          -- belong to that discount ammount group.
          -- Every item can belong to maximum two discount groups!
          --


almostEQCompare :: Double -> Double -> Ordering
almostEQCompare x y
                | x ~== y = EQ
                | x > y   = GT
                | x < y   = LT

grouper :: Double -> Double -> Bool
grouper x y = abs x ~== abs y


printGroups :: (Show a) => [a] -> IO ()
printGroups []     = print "-------------"
printGroups (x:xs) = do putStr "Group: "
                        print x
                        printGroups xs

groupClean :: [[(Double, Int)]] -> [(Double, [Int])]
groupClean []     = []
groupClean (x:xs) = (remDub.foldToTuple) x : groupClean xs
                    where
                          foldToTuple = foldr (\(j,k) (_,l) -> (j, k:(k+1):l)) (0,[])
                          remDub (y, l) = (y, L.nub l)
