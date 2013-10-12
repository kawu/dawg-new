import           System.Exit (exitFailure)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.ST
import           Test.QuickCheck
import qualified Data.Set as S
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic as D
import qualified Data.DAWG.Dynamic.Trie as Trie

-- import           Debug.Trace (trace)
-- import           System.IO.Unsafe (unsafePerformIO)


-- Input parameters.
symNum  = 5     -- Number of distinct symbols.
valNum  = 3     -- Number of distinct values.
wordLen = 5     -- Word length (max).
wordNum = 50    -- Number of words (max).
-- symNum  = 2     -- Number of distinct symbols.
-- valNum  = 1     -- Number of distinct values.
-- wordLen = 2     -- Word length (max).
-- wordNum = 2     -- Number of words (max).


-- | An arbitrary symbol.
arbitrarySym :: Gen Sym
arbitrarySym = choose (0, symNum-1)


-- | An arbitrary value.
arbitraryVal :: Gen Sym
arbitraryVal = choose (0, valNum-1)


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
--     trace (show m ++ "\n===========\n" ++ show d) $
--     trace ("<< " ++ show df ++ " >>") $
    m == d
  where
    m = M.assocs (M.fromList xs)
    d = runST $ D.toList =<< D.fromList xs
--     df = avg . M.elems $ runST $ D.hashFreq =<< D.fromList xs
--     avg xs = fromIntegral (sum xs) / fromIntegral (length xs)


-- | Property: there is one-to-one correspondence between DAWG states
-- and trie subtrees.  Therefore, the size of the set of subtries should
-- be equal to the size of the DAWG.
minimalProp :: Input -> Bool
minimalProp (Input xs) =
--     trace (show (n, e) ++ " <- DAWG /=/ Trie -> " ++ show (n', e')) $
--     unsafePerformIO (D.printDAWG =<< stToIO (D.fromList xs)) `seq`
    n == n' && e == e'
  where
    -- DAWG version
    n  = runST $ D.numStates =<< D.fromList xs
    e  = runST $ D.numEdges  =<< D.fromList xs
    -- Trie version
    ts = S.fromList . Trie.subTries $ Trie.fromList xs
    n' = S.size ts
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
