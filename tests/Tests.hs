import           System.Exit (exitFailure)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.ST
import           Test.QuickCheck
import qualified Data.Set as S
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic as D
import qualified Data.DAWG.Dynamic.Trie as Trie


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


-- | Property: a DAWG dictionary can be used like a map.
contentProp :: Input -> Bool
contentProp (Input xs) =
    m == d
  where
    m = M.assocs (M.fromList xs)
    d = runST $ D.toList =<< D.fromList xs


-- | Property: there is one-to-one correspondence between DAWG states
-- and trie subtrees.  Therefore, the size of the set of subtries should
-- be equal to the size of the DAWG.
minimalProp :: Input -> Bool
minimalProp (Input xs) =
    n == n' && e == e'
  where
    ts = S.fromList . Trie.subTries $ Trie.fromList xs

    n  = runST $ D.numStates =<< D.fromList xs
    n' = S.size ts

    e  = runST $ D.numEdges =<< D.fromList xs
    e' = sum $ map (M.size . Trie.edgeMap) (S.toList ts)


-- | Check property and `exitFailure` on failure.
check :: Testable prop => prop -> IO ()
check prop = quickCheckResult prop >>= \x -> case x of
    Success _ _ _   -> return ()
    _               -> exitFailure

main :: IO ()
main = do
    check contentProp
    check minimalProp
