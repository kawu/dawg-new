-- | The module implements /directed acyclic word graphs/ (DAWGs) internaly
-- represented as /minimal acyclic deterministic finite-state automata/.
-- The implementation provides fast insert and delete operations
-- which can be used to build the DAWG structure incrementaly.


module Data.DAWG.Dynamic
(
-- * DAWG type
  DAWG

-- -- * Query
-- , numStates
-- , numEdges
-- , lookup
-- 
-- -- * Construction
-- , empty
-- , fromList
-- , fromListWith
-- , fromLang
-- 
-- -- ** Insertion
-- , insert
-- , insertWith
-- 
-- -- ** Deletion
-- , delete
-- 
-- -- * Conversion
-- , assocs
-- , keys
-- , elems
) where


import           Prelude hiding (lookup)
import           Control.Monad.ST

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic.Internal as I


-- | Empty DAWG.
data DAWG s = DAWG
    { body  :: I.DFA s
    , root  :: StateID }


-- | An empty DAWG.
empty :: ST s (DAWG s)
empty = do
    (dfa, i) <- I.empty
    return $ DAWG dfa i


-- -- | Number of states in the automaton.
-- numStates :: DAWG a b -> Int
-- numStates = G.size . graph
-- 
-- -- | Number of edges in the automaton.
-- numEdges :: DAWG a b -> Int
-- numEdges = sum . map (length . N.edges) . G.nodes . graph
-- 
-- -- | Insert the (key, value) pair into the DAWG.
-- insert :: (Enum a, Ord b) => [a] -> b -> DAWG a b -> DAWG a b
-- insert xs' y d =
--     let xs = map fromEnum xs'
--         (i, g) = S.runState (insertM xs y $ root d) (graph d)
--     in  DAWG g i
-- {-# INLINE insert #-}
-- {-# SPECIALIZE insert :: Ord b => String -> b -> DAWG Char b -> DAWG Char b #-}
-- 
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
-- 
-- -- | Delete the key from the DAWG.
-- delete :: (Enum a, Ord b) => [a] -> DAWG a b -> DAWG a b
-- delete xs' d =
--     let xs = map fromEnum xs'
--         (i, g) = S.runState (deleteM xs $ root d) (graph d)
--     in  DAWG g i
-- {-# SPECIALIZE delete :: Ord b => String -> DAWG Char b -> DAWG Char b #-}
-- 
-- -- | Find value associated with the key.
-- lookup :: (Enum a, Ord b) => [a] -> DAWG a b -> Maybe b
-- lookup xs' d =
--     let xs = map fromEnum xs'
--     in  S.evalState (lookupM xs $ root d) (graph d)
-- {-# SPECIALIZE lookup :: Ord b => String -> DAWG Char b -> Maybe b #-}
-- 
-- -- | Return all key/value pairs in the DAWG in ascending key order.
-- assocs :: (Enum a, Ord b) => DAWG a b -> [([a], b)]
-- assocs
--     = map (first (map toEnum))
--     . (assocsAcc <$> graph <*> root)
-- {-# SPECIALIZE assocs :: Ord b => DAWG Char b -> [(String, b)] #-}
-- 
-- -- | Return all keys of the DAWG in ascending order.
-- keys :: (Enum a, Ord b) => DAWG a b -> [[a]]
-- keys = map fst . assocs
-- {-# SPECIALIZE keys :: Ord b => DAWG Char b -> [String] #-}
-- 
-- -- | Return all elements of the DAWG in the ascending order of their keys.
-- elems :: Ord b => DAWG a b -> [b]
-- elems = map snd . (assocsAcc <$> graph <*> root)
-- 
-- -- | Construct DAWG from the list of (word, value) pairs.
-- fromList :: (Enum a, Ord b) => [([a], b)] -> DAWG a b
-- fromList xs =
--     let update t (x, v) = insert x v t
--     in  foldl' update empty xs
-- {-# INLINE fromList #-}
-- {-# SPECIALIZE fromList :: Ord b => [(String, b)] -> DAWG Char b #-}
-- 
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
-- 
-- -- | Make DAWG from the list of words.  Annotate each word with
-- -- the @()@ value.
-- fromLang :: Enum a => [[a]] -> DAWG a ()
-- fromLang xs = fromList [(x, ()) | x <- xs]
-- {-# SPECIALIZE fromLang :: [String] -> DAWG Char () #-}
