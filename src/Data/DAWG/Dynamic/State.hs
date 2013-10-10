{-# LANGUAGE RecordWildCards #-}


-- | An automaton state.


module Data.DAWG.Dynamic.State
(
-- * State (ST)
  State (..)
, empty
, null
, mkValue
, mkEdge
, getValue
, setValue
, getTrans
, setTrans
, edgeProd
, overwrite
-- * State (imutable)
, State' (..)
, freeze
, unsafeFreeze
, thaw
, printState'
) where


import           Prelude hiding (null)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.ST
import           Data.STRef
import           Pipes
-- import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types

-- Choice of the low-level transition representation.
-- TODO: could we export appropriate functions from the `Map` submodule
-- and not import the EdgeMap class-module?
import           Data.DAWG.Dynamic.State.EdgeMap.Map (EdgeMap, EdgeMap')
import qualified Data.DAWG.Dynamic.State.EdgeMap.Map as E


---------------------------------------------------
-- ST version of State
---------------------------------------------------


-- | A mutable state of an automaton.
data State s = State {
    -- | A (maybe) value kept in the state.
      valueRef  :: STRef s (Maybe Val)
    -- | A map of outgoing edges.
    , edgeMap   :: EdgeMap s }


-- | An empty state with one ingoing path.
empty :: ST s (State s)
empty = State <$> newSTRef Nothing <*> E.empty


-- | Is the state empty?
null :: State s -> ST s Bool
null State{..} = do
    v <- readSTRef valueRef
    e <- E.null edgeMap
    return $ v == Nothing && e 


-- | A new, only-value state.
mkValue :: Val -> ST s (State s)
mkValue v = State <$> newSTRef (Just v) <*> E.empty


-- | A new, only-edge state.
mkEdge :: Sym -> StateID -> ST s (State s)
mkEdge x y = State <$> newSTRef Nothing <*> E.singleton x y


-- | Get a state value.
getValue :: State s -> ST s (Maybe Val)
getValue State{..} = readSTRef valueRef


-- | Set a state value.
setValue :: Maybe Val -> State s -> ST s ()
setValue v State{..} = writeSTRef valueRef v


-- | Get the outgoing transition on a given symbol or `Nothing`.
getTrans :: Sym -> State s -> ST s (Maybe StateID)
getTrans x State{..} = E.lookup x edgeMap


-- | Set the outgoing transition on a given symbol.
-- Use `Nothing` to delete the transition.
setTrans :: Sym -> Maybe StateID -> State s -> ST s ()
setTrans x mj State{..} = case mj of
    Nothing -> E.delete x edgeMap
    Just j  -> E.insert x j edgeMap


-- | A producer of all state transitions.
edgeProd :: State s -> Producer (Sym, StateID) (ST s) ()
edgeProd State{..} = E.toProd edgeMap


-- | Overwrite the contents of the first state with the contents
-- of the second state.
overwrite :: State s -> State s -> ST s ()
overwrite dst src = do
    -- State'{..} <- unsafeFreeze src
    writeSTRef (valueRef dst) =<< readSTRef (valueRef src)
    E.overwrite (edgeMap dst) (edgeMap src)


---------------------------------------------------
-- Pure version of a state
---------------------------------------------------


-- | A pure version of a state.
data State' = State' {
    -- | A (maybe) value kept in the state.
      value     :: Maybe Val
    -- | A map of outgoing edges.
    , edgeMap'  :: EdgeMap'
    } deriving (Show, Eq, Ord)


-- | Translate state to its pure version.
unsafeFreeze :: State s -> ST s State'
unsafeFreeze State{..} = State'
    <$> readSTRef valueRef
    <*> E.unsafeFreeze edgeMap


-- | Translate state to its pure version.
freeze :: State s -> ST s State'
freeze State{..} = State'
    <$> readSTRef valueRef
    <*> E.freeze edgeMap


-- | Translate state to its pure version.
thaw :: State' -> ST s (State s)
thaw State'{..} = State <$> newSTRef value <*> E.thaw edgeMap'


-- | A helper state printing function.
printState' :: State' -> IO ()
printState' State'{..} = do
    putStr "value:\t" >> print value
    E.printEdgeMap' edgeMap'


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
