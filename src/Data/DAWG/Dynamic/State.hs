{-# LANGUAGE RecordWildCards #-}


-- | An automaton state.


module Data.DAWG.Dynamic.State
( State
, empty
, vstate
, state
, getTrans
, setTrans
, incIngo
, decIngo
) where


import           Data.Int (Int32)
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types


-- | An automaton state.
data State = State {
    -- | A (maybe) value kept in the state.
      value     :: Maybe Val
    -- | A number of ingoing paths (size of a left language).  
    , ingoNum   :: {-# UNPACK #-} !Int32
    -- | A map of outgoing edges.
    , edgeMap   :: M.Map Sym StateID
    } deriving (Show, Eq, Ord)


-- | An empty state.
empty :: State
empty = State Nothing 0 M.empty


-- | A state with initial value.
vstate :: Val -> State
vstate x = State (Just x) 0 M.empty


-- | A generic state construction method.
-- TODO: One ingoing edge?
state :: Maybe Val -> [(Sym, StateID)] -> State
state x = State x 1 . M.fromList


-- | Get the outgoing transition on a given symbol or `Nothing`.
getTrans :: Sym -> State -> Maybe StateID
getTrans x State{..} = M.lookup x edgeMap


-- | Set the outgoing transition on a given symbol.
-- Use `Nothing` to delete the transition.
setTrans :: Sym -> Maybe StateID -> State -> State
setTrans x mj st@State{..} = case mj of
    Nothing -> st { edgeMap = M.delete x edgeMap }
    Just j  -> st { edgeMap = M.insert x j edgeMap }


-- | Increase the number of ingoing paths by one.
incIngo :: State -> State
incIngo = undefined


-- | Decrease the number of ingoing paths by one.
decIngo :: State -> State
decIngo = undefined
