{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}


module Data.DAWG.Dynamic.Internal
(
-- * DFA
  DFAData (..)
, DFA
, insert
, insertRoot
, lookup

-- * DAWG
, DAWG (..)
, numStates
, numEdges
, empty
, assocs
, printDAWG
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<$), (<*>), pure)
import           Control.Monad (forM_, unless)
import           Data.Int (Int32)
import           Data.Maybe (fromJust)
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as U
import qualified Data.Map as M
import           Data.STRef
import           Control.Monad.ST
import           Pipes
import qualified Pipes.Prelude as P
-- import           Pipes.Parse

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
-- (`freeStack`, `stateMap`).  Perhaps all fields should be stored as
-- pointers?  Then we could get rid of the `DFAData`/`DFA` distinction.
data DFAData s = DFAData {

    -- | A vector of DFA states (both active and inactive).  A position of
    -- a state in the vector represents its `StateID`.
    -- TODO: Shouldn't we use an unboxed vector?  See issue #13.
      stateVect :: V.Vector (State s)

    -- | A number of ingoing paths (size of the left language)
    -- for each active state in the automaton.
    , ingoVect  :: U.MVector s Int32

    -- | A stack of free state slots, i.e. inactive states identifiers.
    , freeStack :: P.Stack s StateID

    -- | A map which is used to translate active states (with an exception
    -- of the root) to their corresponding identifiers.  Inactive states
    -- are not kept in the map.
    , stateMap  :: M.Map (State s) StateID }


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
    stateVect' <- growVector stateVect (n+1) N.empty
    ingoVect'  <- U.grow ingoVect (n+1)
    writeSTRef dfa $ dfaData
        { stateVect = stateVect'
        , ingoVect  = ingoVect' }

    let n2 = n*2
    forM_ [n2, n2-1 .. n] $ \i -> do
        P.push i freeStack


-- | Grow an immutable boxed vector by a given number of monadic `a` elements.
-- TODO: Check if it is efficient enough (especially since we are using mutable
-- boxed vector for temporary storage here; maybe list would be sufficient and
-- more safe?).
growVector :: V.Vector a -> Int -> ST s a -> ST s (V.Vector a)
growVector v k mx = do
    let n = V.length v
    mv <- V.unsafeThaw v >>= flip VM.grow k
    forM_ [n, n+1 .. n+k-1] $ \i -> do
        VM.unsafeWrite mv i =<< mx
    V.unsafeFreeze mv


-- | TODO: can try unsafe indexing for efficiency reasons.


-- | Retrieve state with a given identifier.
getState :: DFA s -> StateID -> ST s (State s)
getState dfa i = do
    (V.! i) . stateVect <$> readSTRef dfa


-- | Put state under the given identifier.  The function doesn't update
-- the `stateMap`.
--
-- Question: is the state under the given ID an empty state?
setState :: DFA s -> StateID -> State s -> ST s ()
setState dfa i u = getState dfa i >>= flip N.overwrite u
--     us <- stateVect <$> readSTRef dfa
--     V.write us i u


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
lookupState :: DFA s -> State s -> ST s (Maybe StateID)
lookupState dfa u = M.lookup u . stateMap <$> readSTRef dfa


-- | Add new state into the automaton.  The function assumes,
-- that the state is not a member of the automaton.
addNewState :: DFA s -> State s -> ST s StateID
addNewState dfa u = do
    free <- freeStack <$> readSTRef dfa
    i <- P.pop free >>= \mi -> case mi of
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


-- | It the state non-empty?
emptyState :: DFA s -> StateID -> ST s Bool
emptyState dfa i = N.null =<< getState dfa i


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
getTrans dfa i x = N.getTrans x =<< getState dfa i


-- | Set the outgoing transition.  Use `Nothing` to delete
-- the transition.  The function doesn't update the `stateMap`.
setTrans :: DFA s -> StateID -> Sym -> Maybe StateID -> ST s ()
setTrans dfa i x j = do
    u <- getState dfa i
    N.setTrans x j u


-- | Get a state value.
getValue :: DFA s -> StateID -> ST s (Maybe Val)
getValue dfa i = N.getValue =<< getState dfa i


-- | Set a state value.  The function doesn't update the `stateMap`.
setValue :: DFA s -> StateID -> Maybe Val -> ST s ()
setValue dfa i y = do
    u <- getState dfa i
    N.setValue y u


-- | Add new active state into the automaton.  If its already a member of
-- the automaton, just increase the number of ingoing paths.  Otherwise,
-- one of the inactive states will be used.
forceAddState :: DFA s -> State s -> ST s StateID
forceAddState dfa u = lookupState dfa u >>= \mi -> case mi of
    Nothing -> addNewState dfa u
    Just i  -> i <$ incIngo dfa i


-- | Add new active state into the automaton, unless it is an empty state
-- (there's no point in adding the empty state to the automaton; the given
-- state can be empty e.g. within the context of the `insert` function).
-- If it's already a member of the automaton, just increase the number of
-- ingoing paths.  Otherwise, one of the inactive states will be used.
--
-- The function satisfies the following law:
-- (removeState dfa i >> addState dfa i) == return i
-- unless the state under the @i@ ID is empty.
addState :: DFA s -> StateID -> ST s (Maybe StateID)
addState dfa i = emptyState dfa i `whenFalse`
    (getState dfa i >>= forceAddState dfa)


-- | Compute `Just` value when `True` or `Nothing` otherwise.
whenFalse :: (Functor m, Monad m) => m Bool -> m a -> m (Maybe a)
whenFalse mx m = do
    x <- mx
    if x 
        then return Nothing
        else Just <$> m
{-# INLINE whenFalse #-}


-- | Remove state from the automaton.
--
-- The function satisfies the following law:
-- (removeState dfa i >> addState dfa i) == return i
-- unless the state under the @i@ ID is empty.
removeState :: DFA s -> StateID -> ST s ()
removeState dfa i = do
    dfaData@DFAData{..} <- readSTRef dfa
    P.push i freeStack
    stateMap' <- flip M.delete stateMap <$> getState dfa i
    writeSTRef dfa $ dfaData { stateMap = stateMap' }


-- | Create a new branch in a DFA.  TODO: Can we optimize it?
-- Is it possible to create the entire branch at once?
branch :: DFA s -> [Sym] -> Val -> ST s StateID
branch dfa (x:xs) y = do
    j <- branch dfa xs y
    forceAddState dfa =<< N.state Nothing [(x, j)]
branch dfa [] y = do
    forceAddState dfa =<< N.state (Just y) []


----------------------------------------------------------------------
-- High-level functionality
----------------------------------------------------------------------


-- | Insert a (word, value) pair within a context of a state.
insert :: DFA s -> [Sym] -> Maybe Val -> StateID -> ST s (Maybe StateID)


-- CASE: Non-empty path.
insert dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- case j0 of
        Just j  -> insert dfa xs y j
        Nothing -> T.mapM (branch dfa xs) y
    ingo <- getIngo dfa i0
    go j0 j1 ingo
  where
    go j0 j1 ingo
        | j0 == j1  = do    -- Nothing to do
            return (Just i0)
        | ingo > 1  = do    -- A confluent state
            -- TODO: perhaps it could be done in a more bracket-like style?
            setTrans dfa i0 x j1
            i1 <- addState dfa i0
            setTrans dfa i0 x j0
            decIngo dfa i0
            return i1
        | otherwise = do    -- A non-confluent state
            removeState dfa i0
            setTrans dfa i0 x j1
            addState dfa i0


-- CASE: Empty path.
insert dfa [] y1 i0 = do
    y0 <- getValue dfa i0
    ingo <- getIngo dfa i0
    go y0 ingo
  where
    go y0 ingo
        | y0 == y1  = do    -- Nothing to do
            return (Just i0)
        | ingo > 1  = do    -- A confluent state
            setValue dfa i0 y1
            i1 <- addState dfa i0
            setValue dfa i0 y0
            decIngo dfa i0
            return i1
        | otherwise = do    -- A non-confluent state
            removeState dfa i0
            setValue dfa i0 y1
            addState dfa i0


-- | Insert a (word, value) pair under the assumption, that
-- a state ID represents a DFA root.  In particular, ID
-- of the root doen't change.
--
-- Assumption: the root is not an element of the `stateMap`.
insertRoot :: DFA s -> [Sym] -> Maybe Val -> StateID -> ST s ()
insertRoot dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- case j0 of
        Just j  -> insert dfa xs y j
        Nothing -> T.mapM (branch dfa xs) y
    unless (j0 == j1) $ setTrans dfa i0 x j1
insertRoot dfa [] y1 i0 = setValue dfa i0 y1


-- | Lookup a word in a DFA.
lookup :: DFA s -> [Sym] -> StateID -> ST s (Maybe Val)
lookup dfa (x:xs) i = getTrans dfa i x >>= \mj -> case mj of
    Nothing -> return Nothing
    Just j  -> lookup dfa xs j
lookup dfa [] i     = getValue dfa i


-- | Produce all key/value pairs present in a DFA rooted in a given state.
-- Elements will be reported in an ascending order with respect to keys.
-- To all resultant elements a given prefix will be concatenated.
assocs :: DFA s -> [Sym] -> StateID -> Producer ([Sym], Val) (ST s) ()
assocs dfa xs i = do
    assocsHere
    assocsLower
  where
    assocsHere = do
        mv <- lift $ getValue dfa i
        case mv of
            Nothing -> return ()
            Just v  -> yield (reverse xs, v)
    assocsLower = do
        u <- lift $ getState dfa i
        for (N.transProd u) $ \(x, j) -> do
            assocs dfa (x:xs) j
--         sequence_
--             [ assocs dfa (x:xs) j
--             | (x, j) <- M.toList (N.edgeMap u) ]


----------------------------------------------------------------------
-- DAWG
----------------------------------------------------------------------


-- | A DAWG is a DFA with a distinguished root.
data DAWG s = DAWG
    { dfa   :: DFA s
    , root  :: StateID }


-- | An empty DAWG.
empty :: ST s (DAWG s)
empty = do
    dfaData <- DFAData
        <$> pure V.empty
        <*> U.new 0
        <*> P.empty
        <*> pure M.empty
    dfa  <- newSTRef dfaData
    i    <- forceAddState dfa =<< N.empty
    -- The root must not be in the state map (see the `insertRoot` function).
    modifySTRef dfa $ \x -> x { stateMap = M.empty }
    return $ DAWG dfa i


-- | Number of states in the automaton.
numStates :: DAWG s -> ST s Int
numStates DAWG{..} = (+1) . M.size . stateMap <$> readSTRef dfa


-- | Number of edges in the automaton.
numEdges :: DAWG s -> ST s Int
numEdges DAWG{..} = do
    xs <- M.keys . stateMap <$> readSTRef dfa
    x  <- getState dfa root
    -- return . sum $ map (M.size . N.edgeMap) (x : xs)
    P.length $ mapM_ N.transProd (x:xs)

-- sumP :: (Monad m, Num a) => StateT (Producer a m r) m a
-- sumP = loop 0 where
--     loop x = draw >>= either
--         (\_ -> return x)
--         (\y -> loop $! x + y)


----------------------------------------------------------------------
-- Printing
----------------------------------------------------------------------


-- | A helper DFA printing function.
printDAWG :: DAWG RealWorld -> IO ()
printDAWG DAWG{..} = do
    DFAData{..} <- stToIO $ readSTRef dfa 
    rootState   <- stToIO $ getState dfa root
    forM_ ((rootState, root) : M.toList stateMap) $ \(u, i) -> do
        putStrLn $ "== " ++ show i ++ " =="
        ingo <- stToIO $ getIngo dfa  i
        putStr "ingo:\t" >> print ingo
        N.printState u
