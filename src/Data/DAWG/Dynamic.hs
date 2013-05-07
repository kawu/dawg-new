{-# LANGUAGE RecordWildCards #-}


-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.


module Data.DAWG.Dynamic
(
-- * DAWG type
  DAWG

-- * Query
, numStates
, numEdges
, lookup

-- -- * Construction
, empty

-- ** Insertion
, insert
-- , insertWith

-- ** Deletion
, delete

-- * Conversion

-- ** Pipes
, fromPipe
, toPipe

-- ** Lists
, fromList
, toList
-- , fromListWith

-- * Printing
, printDAWG
) where


import           Prelude hiding (lookup)
import           Control.Monad.ST
import           Control.Proxy
import qualified Control.Proxy.Trans.Writer as W

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic.Internal as I
import           Data.DAWG.Dynamic.Internal
    (DAWG, empty, numStates, numEdges, printDAWG)


-- | Insert (word, value) pair into the DAWG.
insert :: DAWG s -> [Sym] -> Val -> ST s ()
insert dawg xs y = I.insertRoot (I.dfa dawg) xs (Just y) (I.root dawg)


-- -- | Insert with a function, combining new value and old value.
-- -- 'insertWith' f key value d will insert the pair (key, value) into d if
-- -- key does not exist in the DAWG. If the key does exist, the function
-- -- will insert the pair (key, f new_value old_value).
-- insertWith
--     :: (Enum a, Ord b) => (b -> b -> b)
--     -> [a] -> b -> DAWG a b -> DAWG a b
-- insertWith f xs' y d =
--     let xs = map fromEnum xs'
--         (i, g) = S.runState (insertWithM f xs y $ root d) (graph d)
--     in  DAWG g i
-- {-# SPECIALIZE insertWith
--         :: Ord b => (b -> b -> b) -> String -> b
--         -> DAWG Char b -> DAWG Char b #-}


-- | Remove word from the DAWG.
delete :: DAWG s -> [Sym] -> ST s ()
delete dawg xs = I.insertRoot (I.dfa dawg) xs Nothing (I.root dawg)


-- | Lookup a word in the automaton.
lookup :: DAWG s -> [Sym] -> ST s (Maybe Val)
lookup I.DAWG{..} xs = I.lookup dfa xs root


----------------------
-- Pipes
----------------------


-- | Construct DAWG from a pipe producing (word, value) pairs.
fromPipe :: (() -> Producer ProxyFast ([Sym], Val) (ST s) ()) -> ST s (DAWG s)
fromPipe p = do
    dawg <- empty
    runProxy $ p >-> useD (uncurry $ insert dawg)
    return dawg


-- | Return all key/value pairs in the DAWG in ascending key order.
toPipe :: Proxy p => DAWG s -> () -> Producer p ([Sym], Val) (ST s) ()
toPipe I.DAWG{..} = I.assocs dfa [] root


----------------------
-- Lists
----------------------


-- | Construct DAWG from the list of (word, value) pairs.
fromList :: [([Sym], Val)] -> ST s (DAWG s)
fromList = fromPipe . fromListS
-- fromList xs = do
--     dawg <- empty
--     mapM_ (uncurry $ insert dawg) xs
--     return dawg


-- | A version of the `toPipe` function which produces all values at once.
toList :: DAWG s -> ST s [([Sym], Val)]
toList dawg = runProxy $ W.execWriterK $ toPipe dawg >-> toListD


-- -- | Construct DAWG from the list of (word, value) pairs
-- -- with a combining function.  The combining function is
-- -- applied strictly.
-- fromListWith
--     :: (Enum a, Ord b) => (b -> b -> b)
--     -> [([a], b)] -> DAWG a b
-- fromListWith f xs =
--     let update t (x, v) = insertWith f x v t
--     in  foldl' update empty xs
-- {-# SPECIALIZE fromListWith
--         :: Ord b => (b -> b -> b)
--         -> [(String, b)] -> DAWG Char b #-}
