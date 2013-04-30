{-# LANGUAGE RecordWildCards #-}


-- | An automaton state.


module Data.DAWG.Dynamic.State
( State (..)
, empty
, state
, setValue
, getTrans
, setTrans
, incIngo
, isConfl
) where


import           Data.Int (Int32)
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types


-- | An automaton state.
data State = State {
    -- | A (maybe) value kept in the state.
      value     :: Maybe Val
    -- | A number of ingoing paths (size of the left language).
    , ingoNum   :: {-# UNPACK #-} !Int32
    -- | A map of outgoing edges.
    , edgeMap   :: M.Map Sym StateID
    } deriving (Show, Eq, Ord)


-- | An empty state with one ingoing path.
empty :: State
empty = State Nothing 1 M.empty


-- | A state with one ingoing path.
state :: Maybe Val -> [(Sym, StateID)] -> State
state x = State x 1 . M.fromList


-- | Set a state value.
setValue :: Maybe Val -> State -> State
setValue value' u = u { value = value' }


-- | Get the outgoing transition on a given symbol or `Nothing`.
getTrans :: Sym -> State -> Maybe StateID
getTrans x State{..} = M.lookup x edgeMap


-- | Set the outgoing transition on a given symbol.
-- Use `Nothing` to delete the transition.
setTrans :: Sym -> Maybe StateID -> State -> State
setTrans x mj st@State{..} = case mj of
    Nothing -> st { edgeMap = M.delete x edgeMap }
    Just j  -> st { edgeMap = M.insert x j edgeMap }


-- | Increment the number of ingoing paths.
incIngo :: State -> State
incIngo u = u { ingoNum = ingoNum u + 1 }


-- | Is the state a confluent state?  That is, is the number
-- of ingoing paths greater then 1?
isConfl :: State -> Bool
isConfl = (>1) . ingoNum
