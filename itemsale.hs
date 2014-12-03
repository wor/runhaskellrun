-- This is an exercise of solving a simple item price problem.
-- Problem definition:
-- A web shop sells items unspecified items with price of 8.0 (of some
-- monetary unit). But if customer buys multiple different items he/she
-- gets a price discount per item.
-- For two different items the discount is 5% from the total price of these
-- two items.
-- For three different items the discount is 10% from the total price of
-- these three items.
-- For four different items the discount is 20% ...
-- For five different items the discount is 25% ...
-- Customer may buy multiple sets of discounted items and is eligible to
-- maximum discount available to him/her.
-- Final price and the amount of discount must be shown to the customer.
--
-- --
-- Required dependencies:
-- hunit, quickcheck, ieee754
--
-- Usage: :load <this file> in ghci and for example:
--
-- -- Run all tests
-- > runAllTests
--
-- -- Calculate the price of 6 items of which 3 are the same
-- > reportPrice $ takeSets [1,2,3,4,4,4]
--

import Control.Applicative ((<$>),(<*>))
import Control.Monad (join)
import Data.List ( groupBy, sort, nub, (\\), sortBy
                 , mapAccumL, findIndices, delete, splitAt)
import Data.AEq ((~==))
import qualified Test.HUnit as T
import qualified Test.QuickCheck as C


-- | Discounts map: index -> discount percentage
-- As defined by the problem.
-- Note that this is indexed with (!!) which is O(n)
discountsByCount = [0.0, 0.05, 0.10, 0.20, 0.25]

-- | Item price as defined by the problem.
itemPrice = 8.0

--
-- Help functions
--

-- | Unconcatenates lists with a function which takes the list and returns
-- new sublist and what's left of the original list.
unConcatBy              :: ([a] -> ([a], [a])) -> [a] -> [[a]]
unConcatBy _  []        =  []
unConcatBy lp xs        =  ys : unConcatBy lp zs
                           where (ys,zs) = lp xs


-- | Takes at maximum of 5 unique elements from the list and returns the
-- taken elements and whats left of the list in a tuple.
takeSet5                :: (Eq a) => [a] -> ([a], [a])
takeSet5 items          =  (itemset, itemsLeft)
                           where
                               itemset   = take 5 $ nub items
                               itemsLeft = items \\ itemset

-- | Moves a item from sets of size 5 to set of size 3, producing two sets
-- of size 4. The target sets are given as list of index tuples. The fst is
-- the index to set of size 5 and the snd is the index of set of size 3.
-- The index refer to the second [[a]] argument which is the list of item
-- sets.
moveItems               :: (Eq a) => [(Int,Int)] -> [[a]] -> [[a]]
moveItems [] sets       = sets
moveItems (ip:ips) sets = moveItems ips (moveItem ip sets)
    where
        moveItem :: (Eq a) => (Int,Int) -> [[a]] -> [[a]]
        moveItem (i1, i2) sets = replace newSet2 i2 $ replace newSet1 i1 sets
            where
                oldSet1 = sets !! i1
                oldSet2 = sets !! i2
                -- oldSet1 is the larger set so this succeeds always
                item    = head $ oldSet1 \\ oldSet2
                newSet1 = delete item oldSet1
                newSet2 = item : oldSet2
                -- replace item at index at container
                replace it ii c = (++) <$> fst <*> (it:).tail.snd $ splitAt ii c

-- | Forms a list of sublists which contain unique elements up to size
-- 5 from the original list.
takeSets                :: (Eq a) => [a] -> [[a]]
takeSets items          =  moveItems ips sets
    where
        sets = unConcatBy takeSet5 items
        -- Find sets of size 5 and 3 and pair them for item moving, which
        -- is done by moveItems.
        ips  = zip <$> findIndices ((==5).length)
                   <*> findIndices ((==3).length)
                   $ sets


--
-- Price calculation
--

-- | Calculates the price of one set of items.
-- The result is given as tuple of discounted price and the full price
-- without the discount.
calculateSetPrice                :: Int -> (Double, Double)
calculateSetPrice setSize
    | (&&) <$> (>0) <*> (<6) $ setSize = (discountPrice, fullPrice)
    | otherwise                        = error "Unkown set size."
    where
        discountPrice = (1 - discount) * fullPrice
        discount      = discountsByCount !! (setSize - 1)
        fullPrice     = (fromIntegral setSize :: Double) * itemPrice

-- | Calculates price of a set of items.
-- Returns sale price, full price, and saved money.
calculatePrice          :: [[a]] -> (Double, Double, Double)
calculatePrice l        =  (salePrice, fullPrice, saved)
    where
        (salePrice, fullPrice) = foldr foldrFunc (0.0, 0.0) l
        saved                  = fullPrice - salePrice
        foldrFunc :: [a] -> (Double, Double) -> (Double, Double)
        foldrFunc x (discountPrice, fullPrice) = (discountPrice + addDP, fullPrice + addFP)
            where
                (addDP, addFP) = calculateSetPrice $ length x


--
-- Price reporting
--

-- | Prints formated price info for a given list of item sets.
--
-- Example call:
--
-- > reportPrice $ takeSets [1,2,3,4,10,10,2,8]
--
reportPrice             :: [[a]] -> IO ()
reportPrice set         =  putStrLn ("Bought " ++ itemCount ++ " items.")
                           >> putStrLn ("Discount price: " ++ (show $ discountPrice))
                           >> putStrLn ("Full price: "     ++ (show $ fullPrice))
                           >> putStrLn ("You save: "       ++ (show $ save))
    where
        itemCount                        = show $ length $ concat $ set
        (discountPrice, fullPrice, save) = calculatePrice set


--
-- HUnit Tests
-- TODO: clean a bit more
--

data UTest = UTest { uItems           :: [Int]
                   , uSetSizes        :: [Int]
                   , uSucceeds        :: Bool
                   , uDiscountPrice   :: Double
                   , uFullPrice       :: Double
                   , uDiffPrice       :: Double
                   } deriving (Show)

createTestCaseData :: [Int] -> [Int] -> Bool -> UTest
createTestCaseData items setSizes succeeds =
        UTest { uItems         = items
              , uSetSizes      = setSizes
              , uSucceeds      = succeeds
              , uDiscountPrice = discPrice
              , uFullPrice     = sum $ fullPrice setSizes
              , uDiffPrice     = diffPrice
              }
        where
            discPrice = sum $ zipWith (*) <$> map ((1-).d) <*> fullPrice $ setSizes
            fullPrice = map ((*itemPrice).fromIntegral)
            diffPrice = subtract discPrice $ sum $ fullPrice setSizes
            d         = ((!!) discountsByCount) <$> (subtract 1)

-- | Creates a test which tests that items stay the same, that formed set
-- count is right and finally that the formed set lengths are right.
createTestCase :: UTest -> String -> T.Test
createTestCase td prefix = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = prefix
        input         = (isSame, subCount, subLengths)
        expected      = (uSucceeds td, length $ uSetSizes td, uSetSizes td)
        --
        result        = takeSets $ uItems td
        isSame        = (sort $ concat result) == (sort $ uItems td)
        subCount      = length result
        subLengths    = sort $ length <$> result

createUPriceTest :: UTest -> String -> T.Test
createUPriceTest td prefix = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = prefix
        input         = calculatePrice $ takeSets $ uItems td
        expected      = (uDiscountPrice td, uFullPrice td, uDiffPrice td)


testData1   = createTestCaseData [1,1,1,1,1,1] [1,1,1,1,1,1] True
uTest1      = createTestCase testData1 "takeSets test 1"
uPriceTest1 = createUPriceTest testData1 "price test 1"

testData2   = createTestCaseData [1,2] [2] True
uTest2      = createTestCase testData2 "takeSets test 2"
uPriceTest2 = createUPriceTest testData2 "price test 2"

testData3   = createTestCaseData [1,2,3,3,3,3,4,5,6,2,5,8,7] [1,2,5,5] True
uTest3      = createTestCase testData2 "takeSets test 3"
uPriceTest3 = createUPriceTest testData3 "price test 3"

testData4   = createTestCaseData [1,2,3,4,5,1,2,3,4,1,2,3] [4,4,4] True
uTest4      = createTestCase testData2 "takeSets test 4"
uPriceTest4 = createUPriceTest testData4 "price test 4"

testData5   = createTestCaseData (double $ uItems testData4)
                                 (double $ uSetSizes testData4) True
    where
        double = join (++)
uTest5      = createTestCase testData2 "takeSets test 5"
uPriceTest5 = createUPriceTest testData5 "price test 5"


allTests = T.TestList [T.TestLabel "PriceTest1"    uPriceTest1
                      ,T.TestLabel "PriceTest2"    uPriceTest2
                      ,T.TestLabel "PriceTest3"    uPriceTest3
                      ,T.TestLabel "PriceTest4"    uPriceTest4
                      ,T.TestLabel "PriceTest5"    uPriceTest5
                      ,T.TestLabel "takeSetsTest1" uTest1
                      ,T.TestLabel "takeSetsTest2" uTest2
                      ,T.TestLabel "takeSetsTest3" uTest3
                      ,T.TestLabel "takeSetsTest4" uTest4
                      ,T.TestLabel "takeSetsTest5" uTest5
                      ]

--
-- QuickTests
--

-- | Checks that takeSets produces sets ordered by length, largest first.
-- This is a QuickCheck check and can be run with C.quickCheck
prop_takeSetsSort            :: (Eq a) => [a] -> Bool
prop_takeSetsSort items      =  sortBy lsort sets == sets
                                where
                                    sets = takeSets items
                                    lsort :: [a] -> [a] -> Ordering
                                    lsort x y
                                         | length x > length y  = GT
                                         | length x < length y  = LT
                                         | length x == length y = EQ


runAllTests = do T.runTestTT allTests
