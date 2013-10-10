-- | The module provides an abstraction over transition maps
-- from alphabet symbols to state identifiers.


module Data.DAWG.Dynamic.State.EdgeMap
( EdgeMap (..)
) where


import           Control.Monad.ST
import           Pipes

import           Data.DAWG.Dynamic.Types


-- | Abstraction over transition maps from alphabet symbols
-- to node identifiers.
class EdgeMap t where
    -- | Empty transition map.
    empty       :: ST s (t s)
    -- | Single-element transition map.
    singleton   :: Sym -> StateID -> ST s (t s)
    -- | Is the map empty?
    null        :: t s -> ST s Bool
    -- | Lookup symbol in the map.
    lookup      :: Sym -> t s -> ST s (Maybe StateID)
    -- | Insert element to the map.
    insert      :: Sym -> StateID -> t s -> ST s ()
    -- | Remove element from the map.
    delete      :: Sym -> t s -> ST s ()
    -- | Translate map into a producer.
    toProd      :: t s -> Producer (Sym, StateID) (ST s) ()


-- -- | A pure version of 
-- class (Eq t, Ord t) => EdgeMapP t where


--     -- | Find index of the symbol.
--     index       :: Sym -> t s -> ST s (Maybe Int)
--     -- | Select a (symbol, ID) pair by index of its position in the map.
--     byIndex     :: Int -> t -> Maybe (Sym, ID)
--     -- | Construct transition map from a list.
--     fromList    :: [(Sym, ID)] -> t
--     -- | Translate transition map into a list.
--     toList      :: t -> [(Sym, ID)]
