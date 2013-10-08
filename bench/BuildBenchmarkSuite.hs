{-# LANGUAGE RecordWildCards #-}


import           Criterion.Config
import           Criterion.Main
import           Control.Monad (replicateM, replicateM_)
import           Control.Monad.ST
import           Pipes
import qualified Pipes.Prelude as P
import qualified System.Random.MWC as R
import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic as D


-- | A language parameters.
data Param = Param
    { symNum    :: Int      -- ^ Size of alphabet
    , valNum    :: Int      -- ^ Number of distinct values
    , maxLen    :: Int      -- ^ Maximum length of a word
    -- | Number of words (ignored when using the regular dictionary
    -- construction method).
    , wordNum   :: Int      
    } deriving (Show)


-- | A random dictionary producer given configuration.
randomDict :: Param -> Producer ([Sym], Val) (ST s) ()
randomDict Param{..} = do
    gen <- lift R.create
    replicateM_ wordNum $ do
        -- Random word
        x <- lift $ do
            n <- R.uniformR (1, maxLen) gen
            replicateM n $ R.uniformR (1, symNum) gen
        -- Random value
        v <- lift $ R.uniformR (1, valNum) gen
        yield (x, v)


-- | A length of a random dictionary.
randomLen :: Param -> Int
randomLen prm = runST $ P.length $ randomDict prm


-- | Build DAWG from a random dictionary and return the number of states.
randomDAWG :: Param -> Int
randomDAWG prm = runST $ D.numStates =<< D.fromPipe (randomDict prm)


-- | A regular dictionary.
regularDict :: Param -> [([Sym], Val)]
regularDict Param{..} = 
    zip xs vs
  where
    vs = cycle [1 .. valNum]
    xs = concat [listWords n | n <- [1 .. maxLen]]
    listWords n = sequence $ replicate n [1 .. symNum]


-- | A length of a regular dictionary.
regularLen :: Param -> Int
regularLen = length . regularDict


-- | Build DAWG from a regular dictionary and return the number of states.
regularDAWG :: Param -> Int
regularDAWG prm = runST $ D.numStates =<< D.fromList (regularDict prm)


-- | Benchmark suite.
benchmarks :: [Benchmark]
benchmarks =
    [
      let prm = Param 10 3 10 10000
      in  bgroup "Random"
        [ bench ("Dict: " ++ show prm) (whnf randomLen prm)
        , bench ("DAWG: " ++ show prm) (whnf randomDAWG prm) ]
    , let prm = Param 30 3 3 (-1)
      in  bgroup "Regular"
        [ bench ("Dict: " ++ show prm) (whnf regularLen prm)
        , bench ("DAWG: " ++ show prm) (whnf regularDAWG prm) ]
    ]
 

-- | Benchmark configuration.
benchConfig :: Config
benchConfig = defaultConfig {
    -- | Always GC between runs.
      cfgPerformGC = ljust True
    }


-- | Entry point.
main :: IO ()
main = defaultMainWith benchConfig (return ()) benchmarks
