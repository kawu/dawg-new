-- | Basic data types.


module Data.DAWG.Dynamic.Types
( Sym
, Val
, StateID
) where


-- | Alphabet symbol (or letter).
type Sym = Int


-- | A value kept in automata nodes.
type Val = Int


-- | A state identifier.  TODO: In some places
-- we should use smaller types (e.g. Int32).
type StateID = Int


