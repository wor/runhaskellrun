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
-- Usage: :load <this file> in ghci and for example:
--
-- -- Run all tests
-- > runAllTests
--
-- -- Calculate the price of 6 items of which 3 are the same
-- > reportPrice $ takeSets [1,2,3,4,4,4]
--

import Control.Applicative ((<$>),(<*>))
import Data.List (groupBy, sort, nub, (\\), sortBy)

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

-- | Forms a list of sublists which contain unique elements up to size
-- 5 from the original list.
takeSets                :: (Eq a) => [a] -> [[a]]
takeSets                =  unConcatBy takeSet5


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
-- TODO: whoah these need cleaning..
--

itemsTV1    = [1,1,1,1,1,1]
itemsTVEx1  = (True, 6, [1,1,1,1,1,1])
itemsTVExP1 = (discountPrice, fullPrice, difference)
    where
        discountPrice = fromIntegral (length itemsTV1) * itemPrice
        fullPrice     = fromIntegral (sum itemsTV1) * itemPrice
        difference    = fullPrice - discountPrice

itemsTV2    = [1,2]
itemsTVEx2  = (True, 1, [2])
itemsTVExP2 = (discountPrice, fullPrice, difference)
    where
        discountPrice = 0.95*2*itemPrice
        fullPrice     = sum [2] * itemPrice
        difference    = fullPrice - discountPrice

itemsTV3    = [1,2,3,3,3,3,4,5,6,2,5,8,7]
itemsTVEx3  = (True, 4, [1,2,5,5])
itemsTVExP3 = (discountPrice, fullPrice, difference)
    where
        discountPrice = itemPrice+(0.95*2*itemPrice)+2*(0.75*5*itemPrice)
        fullPrice     = (sum [1,2,5,5] * itemPrice)
        difference    = fullPrice - discountPrice

-- Testcase can be run with T.runTestTT

-- | Tests that items stay the same, that formed set count is right and
-- finally that formed set lengths are right.
takeSetsTest1 :: T.Test
takeSetsTest1 = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = "takeSets test 1"
        input         = (isSame, subCount, subLengths)
        expected      = itemsTVEx1
        --
        result        = takeSets itemsTV1
        isSame        = (sort $ concat result) == (sort itemsTV1)
        subCount      = length result
        subLengths    = sort $ length <$> result

takeSetsTest2 :: T.Test
takeSetsTest2 = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = "takeSets test 2"
        input         = (isSame, subCount, subLengths)
        expected      = itemsTVEx2
        --
        result        = takeSets itemsTV2
        isSame        = (sort $ concat result) == (sort itemsTV2)
        subCount      = length result
        subLengths    = sort $ length <$> result

takeSetsTest3 :: T.Test
takeSetsTest3 = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = "takeSets test 3"
        input         = (isSame, subCount, subLengths)
        expected      = itemsTVEx3
        --
        result        = takeSets itemsTV3
        isSame        = (sort $ concat result) == (sort itemsTV3)
        subCount      = length result
        subLengths    = sort $ length <$> result

-- | Test calculated prices for the items
priceTest1 :: T.Test
priceTest1 = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = "priceTests test 1"
        input         = calculatePrice $ takeSets itemsTV1
        expected      = itemsTVExP1

priceTest2 :: T.Test
priceTest2 = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = "priceTests test 2"
        input         = calculatePrice $ takeSets itemsTV2
        expected      = itemsTVExP2

priceTest3 :: T.Test
priceTest3 = T.TestCase $ T.assertEqual messagePrefix expected input
    where
        messagePrefix = "priceTests test 3"
        input         = calculatePrice $ takeSets itemsTV3
        expected      = itemsTVExP3


allTests = T.TestList [T.TestLabel "priceTest1"    priceTest1
                      ,T.TestLabel "priceTest2"    priceTest2
                      ,T.TestLabel "priceTest3"    priceTest3
                      ,T.TestLabel "takeSetsTest1" takeSetsTest1
                      ,T.TestLabel "takeSetsTest2" takeSetsTest2
                      ,T.TestLabel "takeSetsTest3" takeSetsTest3
                      ]

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
