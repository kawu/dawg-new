import           System.Exit (exitFailure)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.ST
import           Test.QuickCheck
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic as D


-- Input parameters.
symNum  = 3     -- Number of distinct symbols.
valNum  = 3     -- Number of distinct values.
wordLen = 5     -- Word length (max).
wordNum = 50    -- Number of words (max).


-- | An arbitrary symbol.
arbitrarySym :: Gen Sym
arbitrarySym = choose (0, symNum)


-- | An arbitrary value.
arbitraryVal :: Gen Sym
arbitraryVal = choose (0, valNum)


-- | An arbitrary word.
arbitraryWord :: Gen [Sym]
arbitraryWord = do
    k <- choose (0, wordLen)
    vectorOf k arbitrarySym


-- | Test input.
data Input = Input [([Sym], Val)] deriving (Show)


instance Arbitrary Input where
    arbitrary = do
        k   <- choose (0, wordNum)
        xs  <- vectorOf k arbitraryWord
        ys  <- vectorOf k arbitraryVal
        return $ Input (zip xs ys)


-- | Property: the dictionary can be used like a map. 
contentProp :: Input -> Bool
contentProp (Input xs) =
    m == d
  where
    m = M.assocs (M.fromList xs)
    d = runST $ D.toList =<< D.fromList xs


-- | Check property and `exitFailure` on failure.
check :: Testable prop => prop -> IO ()
check prop = quickCheckResult prop >>= \x -> case x of
    Success _ _ _   -> return ()
    _               -> exitFailure

main :: IO ()
main = do
    check contentProp
