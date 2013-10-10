{-# LANGUAGE RecordWildCards #-}


-- | An automaton state.


module Data.DAWG.Dynamic.State
(
-- * State (ST)
  State (..)
, empty
, null
, state
, getValue
, setValue
, getTrans
, setTrans
, transProd
, overwrite
-- * State (pure)
, StateP (..)
, fromPure
, toPure
, printState
) where


import           Prelude hiding (null)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.ST
import           Data.STRef
import           Pipes
import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types


-- | A mutable state of an automaton.
data State s = State {
    -- | A (maybe) value kept in the state.
      valueRef      :: STRef s (Maybe Val)
    -- | A map of outgoing edges.  TODO: make some real
    -- implementation here.
    , edgeMapRef    :: STRef s (M.Map Sym StateID) }


-- | An empty state with one ingoing path.
empty :: ST s (State s)
empty = State <$> newSTRef Nothing <*> newSTRef M.empty


-- | Is the state empty?
null :: State s -> ST s Bool
null State{..} = do
    v <- readSTRef valueRef
    e <- readSTRef edgeMapRef
    return $ v == Nothing && M.null e 


-- | A state with one ingoing path.
state :: Maybe Val -> [(Sym, StateID)] -> ST s (State s)
state v xs = State <$> newSTRef v <*> newSTRef (M.fromList xs)


-- | Get a state value.
getValue :: State s -> ST s (Maybe Val)
getValue State{..} = readSTRef valueRef


-- | Set a state value.
setValue :: Maybe Val -> State s -> ST s ()
setValue v State{..} = writeSTRef valueRef v


-- | Get the outgoing transition on a given symbol or `Nothing`.
getTrans :: Sym -> State s -> ST s (Maybe StateID)
getTrans x State{..} = M.lookup x <$> readSTRef edgeMapRef


-- | Set the outgoing transition on a given symbol.
-- Use `Nothing` to delete the transition.
setTrans :: Sym -> Maybe StateID -> State s -> ST s ()
setTrans x mj State{..} = case mj of
    Nothing -> modifySTRef edgeMapRef $ M.delete x
    Just j  -> modifySTRef edgeMapRef $ M.insert x j


-- | A producer of all state transitions.
transProd :: State s -> Producer (Sym, StateID) (ST s) ()
transProd State{..} = do
    xs <- lift $ M.toList <$> readSTRef edgeMapRef
    mapM_ yield xs


-- | Overwrite the contents of the first state with the contents
-- of the second state.
overwrite :: State s -> State s -> ST s ()
overwrite State{..} src = do
    StateP{..} <- toPure src
    writeSTRef valueRef value
    writeSTRef edgeMapRef edgeMap


---------------------------------------------------
-- Pure version of a state
---------------------------------------------------


-- | A pure version of a state.
data StateP = StateP {
    -- | A (maybe) value kept in the state.
      value     :: Maybe Val
    -- | A map of outgoing edges.  TODO: make some real
    -- implementation here.
    , edgeMap   :: M.Map Sym StateID
    } deriving (Show, Eq, Ord)


-- | Translate state to its pure version.
fromPure :: StateP -> ST s (State s)
fromPure StateP{..} = State <$> newSTRef value <*> newSTRef edgeMap


-- | Translate state to its pure version.
toPure :: State s -> ST s StateP
toPure State{..} = StateP <$> readSTRef valueRef <*> readSTRef edgeMapRef


-- | A helper state printing function.
printState :: StateP -> IO ()
printState StateP{..} = do
    putStr "value:\t" >> print value
    mapM_ print (M.toList edgeMap)


---------------------------------------------------
-- Pure, old version of the module
---------------------------------------------------


-- -- | An automaton state.
-- data State = State {
--     -- | A (maybe) value kept in the state.
--       value     :: Maybe Val
--     -- | A map of outgoing edges.
--     , edgeMap   :: M.Map Sym StateID
--     } deriving (Show, Eq, Ord)
-- 
-- 
-- -- | An empty state with one ingoing path.
-- empty :: State
-- empty = State Nothing M.empty
-- 
-- 
-- -- | Is the state empty?
-- null :: State -> Bool
-- null (State Nothing m) = M.null m
-- null _                 = False
-- 
-- 
-- -- | A state with one ingoing path.
-- state :: Maybe Val -> [(Sym, StateID)] -> State
-- state x = State x . M.fromList
-- 
-- 
-- -- | Set a state value.
-- setValue :: Maybe Val -> State -> State
-- setValue value' u = u { value = value' }
-- 
-- 
-- -- | Get the outgoing transition on a given symbol or `Nothing`.
-- getTrans :: Sym -> State -> Maybe StateID
-- getTrans x State{..} = M.lookup x edgeMap
-- 
-- 
-- -- | Set the outgoing transition on a given symbol.
-- -- Use `Nothing` to delete the transition.
-- setTrans :: Sym -> Maybe StateID -> State -> State
-- setTrans x mj st@State{..} = case mj of
--     Nothing -> st { edgeMap = M.delete x edgeMap }
--     Just j  -> st { edgeMap = M.insert x j edgeMap }
-- 
-- 
-- -- | A helper state printing function.
-- printState :: State -> IO ()
-- printState State{..} = do
--     putStr "value:\t" >> print value
--     mapM_ print (M.toList edgeMap)
