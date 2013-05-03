{-# LANGUAGE RecordWildCards #-}


-- | An automaton state.


module Data.DAWG.Dynamic.State
( State (..)
, empty
, null
, state
, setValue
, getTrans
, setTrans
, printState
) where


import           Prelude hiding (null)
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types


-- | An automaton state.
data State = State {
    -- | A (maybe) value kept in the state.
      value     :: Maybe Val
    -- | A map of outgoing edges.
    , edgeMap   :: M.Map Sym StateID
    } deriving (Show, Eq, Ord)


-- | An empty state with one ingoing path.
empty :: State
empty = State Nothing M.empty


-- | Is the state empty?
null :: State -> Bool
null (State Nothing m) = M.null m
null _                 = False


-- | A state with one ingoing path.
state :: Maybe Val -> [(Sym, StateID)] -> State
state x = State x . M.fromList


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


-- | A helper state printing function.
printState :: State -> IO ()
printState State{..} = do
    putStr "value:\t" >> print value
    mapM_ print (M.toList edgeMap)
