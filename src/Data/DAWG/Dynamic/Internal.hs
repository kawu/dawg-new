{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Data.DAWG.Dynamic.Internal
( DFAData (..)
, DFA
, empty
, insert
, insertRoot
, lookup
, printDFA
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<$), (<*>), pure)
import           Control.Monad (void, forM_)
import           Data.Int (Int32)
import           Data.Maybe (fromJust)
import qualified Data.Traversable as T
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Map as M
import           Data.STRef
import           Control.Monad.ST

import           Data.DAWG.Dynamic.Types
import           Data.DAWG.Dynamic.State (State)
import qualified Data.DAWG.Dynamic.State as N
import qualified Data.DAWG.Dynamic.Stack as P


----------------------------------------------------------------------
-- DFA state monad
----------------------------------------------------------------------


-- | DFA consists of a set of states and transitions between individual states.
-- Each state may be active (reachable from the root state) or inactive
-- (otherwise).  DFA data can be used within the context of the `ST` monad
-- (hence the @s@ parameter).
--
-- TODO: This representation is a bit inconsistent.  Some fields are
-- represented directly (`stateVect`, `ingoVect`), other as pointers
-- (`freeStack`, `stateMap`).  Perhaps all fields should be stored
-- as pointers?  Then we could get rid of the `DFADAta`/`DFA`
-- distinction.
data DFAData s = DFAData {
    -- | A vector of DFA states (both active and inactive).  A position of
    -- a state in the vector represents its `StateID`.
      stateVect :: V.MVector s State
    -- | A number of ingoing paths (size of the left language)
    -- for each active state in the automaton.
    , ingoVect  :: U.MVector s Int32
    -- | A stack of free state slots, i.e. inactive states identifiers.
    , freeStack :: P.Stack s StateID
    -- | A map which is used to translate active states to their
    -- corresponding identifiers.  Inactive states are not kept
    -- it the map.
    -- TODO: The root state can be treated in a special way.  In particular,
    -- it should not be an element of the `stateMap`.
    , stateMap  :: M.Map State StateID }


-- | A DFA reference.
type DFA s = STRef s (DFAData s)


----------------------------------------------------------------------
-- Low-level operations (work on both active and inactive states).
----------------------------------------------------------------------


-- | Grow DFA with new inactive states.
grow :: DFA s -> ST s ()
grow dfa = do
    dfaData@DFAData{..} <- readSTRef dfa
    let n = V.length stateVect

    -- Use n+1 to handle empty dfa.
    stateVect' <- V.grow stateVect (n + 1)
    ingoVect'  <- U.grow ingoVect  (n + 1)
    writeSTRef dfa $ dfaData
        { stateVect = stateVect'
        , ingoVect  = ingoVect' }

    let n2 = n*2
    forM_ [n2, n2-1 .. n] $ \i -> do
        P.push i freeStack


-- | Retrieve state with a given identifier.
getState :: DFA s -> StateID -> ST s State
getState dfa i = do
    us <- stateVect <$> readSTRef dfa
    V.read us i


-- | Set state with a given identifier.
setState :: DFA s -> StateID -> State -> ST s ()
setState dfa i u = do
    us <- stateVect <$> readSTRef dfa
    V.write us i u


-- | Get a number of ingoing paths for a given state.
getIngo :: DFA s -> StateID -> ST s Int
getIngo dfa i = do
    v <- ingoVect <$> readSTRef dfa
    fromIntegral <$> U.read v i


-- | Get a number of ingoing paths for a given state.
setIngo :: DFA s -> StateID -> Int -> ST s ()
setIngo dfa i x = do
    v <- ingoVect <$> readSTRef dfa
    U.write v i (fromIntegral x)
-- | Lookup state identifier in a DFA.
lookupState :: DFA s -> State -> ST s (Maybe StateID)
lookupState dfa u = M.lookup u . stateMap <$> readSTRef dfa


-- | Add new state into the automaton.  The function assumes,
-- that the state is not a member of the automaton.
_addState :: DFA s -> State -> ST s StateID
_addState dfa u = do
    free <- freeStack <$> readSTRef dfa
    i <- P.pop free >>= \case
        Just i  -> return i
        Nothing -> do
            grow dfa
            fromJust <$> P.pop free
    setState dfa i u
    setIngo  dfa i 1
    modifySTRef dfa $ \dfaData ->
        let stateMap' = M.insert u i (stateMap dfaData)
        in  dfaData { stateMap = stateMap' }
    return i


----------------------------------------------------------------------
-- Medium-level interface (an additional layer between the low-level
-- and the high-level functionality).
----------------------------------------------------------------------


-- | Incerement the number of ingoing paths for a given state.
incIngo :: DFA s -> StateID -> ST s ()
incIngo dfa i = do
    v <- ingoVect <$> readSTRef dfa
    x <- U.read v i
    U.write v i (x + 1)


-- | Incerement the number of ingoing paths for a given state.
decIngo :: DFA s -> StateID -> ST s ()
decIngo dfa i = do
    v <- ingoVect <$> readSTRef dfa
    x <- U.read v i
    U.write v i (x - 1)


-- | Get the outgoing transition.  Return `Nothin` if there is
-- no transition on a given symbol.
getTrans :: DFA s -> StateID -> Sym -> ST s (Maybe StateID)
getTrans dfa i x = N.getTrans x <$> getState dfa i


-- | Set the outgoing transition.  Use `Nothing` to delete
-- the transition.
setTrans :: DFA s -> StateID -> Sym -> Maybe StateID -> ST s ()
setTrans dfa i x j = do
    u <- getState dfa i
    setState dfa i $ N.setTrans x j u


-- | Get a state value.
getValue :: DFA s -> StateID -> ST s (Maybe Val)
getValue dfa i = N.value <$> getState dfa i


-- | Set a state value.
setValue :: DFA s -> StateID -> Maybe Val -> ST s ()
setValue dfa i y = do
    u <- getState dfa i
    setState dfa i $ N.setValue y u


-- | Add new active state into the automaton.  If its already a member of
-- the automaton, just increase the number of ingoing paths.  Otherwise,
-- one of the inactive states will be used.
--
-- TODO: Rename to addState' or something similar.
addState :: DFA s -> State -> ST s StateID
addState dfa u = lookupState dfa u >>= \case
    Nothing -> _addState dfa u
    Just i  -> i <$ incIngo dfa i


-- | Add new active state into the automaton.  If its already a member of
-- the automaton, just increase the number of ingoing paths.  Otherwise,
-- one of the inactive states will be used.
--
-- TODO: Rename to addState.
-- TODO: Maybe we should check here, it the state is useful?
--
-- The function satisfies the following law:
-- (removeState dfa i >> addStateID dfa i) == return i
addStateID :: DFA s -> StateID -> ST s StateID
addStateID dfa i = getState dfa i >>= addState dfa


-- | Remove state from the automaton.
--
-- The function satisfies the following law:
-- (removeState dfa i >> addStateID dfa i) == return i
removeState :: DFA s -> StateID -> ST s ()
removeState dfa i = do
    dfaData@DFAData{..} <- readSTRef dfa
    P.push i freeStack
    stateMap' <- flip M.delete stateMap <$> getState dfa i
    writeSTRef dfa $ dfaData { stateMap = stateMap' }


-- | Create a new branch in a DFA.
branch :: DFA s -> [Sym] -> Val -> ST s StateID
branch dfa (x:xs) y = do
    j <- branch dfa xs y
    addState dfa $ N.state Nothing [(x, j)]
branch dfa [] y = do
    addState dfa $ N.state (Just y) []


----------------------------------------------------------------------
-- High-level functionality
----------------------------------------------------------------------


-- | An empty DFA with one root state ID.
empty :: ST s (DFA s, StateID)
empty = do
    dfaData <- DFAData
        <$> V.new 0
        <*> U.new 0
        <*> P.empty
        <*> pure M.empty
    dfa  <- newSTRef dfaData
    i    <- addState dfa N.empty
    return (dfa, i)


-- | Insert a (word, value) pair within a context of a state.
insert :: DFA s -> [Sym] -> Maybe Val -> StateID -> ST s (Maybe StateID)
insert dfa xs y i = do
    ingoNum <- getIngo dfa i
    let doInsert = if ingoNum > 1
            then insertConfl
            else insertNonConfl
    doInsert dfa xs y i


-- | Compute `Just` value when `True` or `Nothing` otherwise.
whenTrue :: Monad m => Bool -> m a -> m (Just a)
whenTrue x m = if x
    then Just <$> m
    else return Nothing


-- | Insert a (word, value) pair within a context of a confluent state.
-- TODO: Cut off computation when (j0 == j1).
insertConfl :: DFA s -> [Sym] -> Maybe Val -> StateID -> ST s (Maybe StateID)


-- CASE: Non-empty path.
-- TODO: Cut off computation when j0 == j1.
insertConfl dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- case j0 of
        Just j  -> insertConfl dfa xs y j
        Nothing -> T.mapM (branch dfa xs) y
    if j0 == j1 then do
        return Nothing
    else do
        setTrans dfa i0 x j1
        i1 <- usefulState dfa i0 `whenTrue` addStateID dfa i0
        setTrans dfa i0 x j0
        decIngo dfa i0
        return i1


-- CASE: Empty path.
-- TODO: Cut off computation when y0 == y1.
insertConfl dfa [] y1 i0 = do
    y0 <- getValue dfa i0
    setValue dfa i0 y1
    i1 <- usefulState dfa i0 `whenTrue` addStateID dfa i0
    setValue dfa i0 y0
    decIngo dfa i0
    return i1


-- | Insert a (word, value) pair within a context of a non-confluent state.
insertNonConfl :: DFA s -> [Sym] -> Val -> StateID -> ST s StateID


-- CASE: Non-empty path.
insertNonConfl dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- case j0 of
        Just j  -> insert dfa xs y j
        Nothing -> T.mapM (branch dfa xs) y
    if j0 == j1 then do
        return Nothing
    else do
        removeState dfa i0
        setTrans dfa i0 x j1
        usefulState dfa i0 `whenTrue` addStateID dfa i0


-- CASE: Empty path.
-- TODO: Cut off computation when y0 == y1.
insertNonConfl dfa [] y1 i0 = do
    _y0 <- getValue dfa i0
    removeState dfa i0
    setValue dfa i0 y1
    usefulState dfa i0 `whenTrue` addStateID dfa i0


-- | Insert a (word, value) pair under the assumption, that
-- a state ID represents a DFA root.  In particular, ID
-- of the root doen't change.
-- TODO: Rename this function to insert?
insertRoot :: DFA s -> [Sym] -> Val -> StateID -> ST s ()
insertRoot dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- case j0 of
        Just j  -> insert dfa xs y j
        Nothing -> T.mapM (branch dfa xs) y
    unless (j0 == j1) $ setTrans dfa i0 x j1
insertRoot dfa [] y1 i0 = setValue dfa i0 y1


-- | Lookup a word in a DFA.
lookup :: DFA s -> [Sym] -> StateID -> ST s (Maybe Val)
lookup dfa (x:xs) i = getTrans dfa i x >>= \case
    Nothing -> return Nothing
    Just j  -> lookup dfa xs j
lookup dfa [] i     = getValue dfa i


----------------------------------------------------------------------
-- Printing
----------------------------------------------------------------------


-- | A helper DFA printing function.
printDFA :: DFA RealWorld -> IO ()
printDFA dfa = do
    -- TODO: Print size of the left-language
    DFAData{..} <- stToIO $ readSTRef dfa
    forM_ (M.toList stateMap) $ \(state, i) -> do
        putStrLn $ "== " ++ show i ++ " =="
        N.printState state
