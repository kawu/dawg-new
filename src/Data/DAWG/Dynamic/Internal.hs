{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


{-
 - Dodanie nowego słowa do słownika:
 -
 -
 - DEFINICJA: Stan jest konfluentny wtw. gdy liczba wchodzących
 - do niego ścieżek jest większa niż 1 (alternatywnie, gdy jego
 - lewy język ma rozmiar większy niż 1).
 - I NIE MA TUTAJ ZNACZENIA LICZBA WCHODZĄCYCH KRAWEDZI!
 -
 -
 - FAKT: Stan klonujemy wtw stan jest konfluentny.
 -
 - 
 - a) Stan niekonfluentny
 - 
 - Co robimy, gdy stan nie jest konfluentny?
 - Stan ten ma jedną ścieżkę wchodzącą.
 - Nie kopiujemy go.  Teoretycznie, powinniśmy usunąć
 - oryginalny stan i dodać zmodyfikowany, ale ponieważ
 - oryginalny stan nie będzie już potrzebny, możemy
 - wykorzystać go do przechowania informacji nowego
 - stanu.  Jeśli chodzi o krawędzie prowadzące do
 - oryginalnego stanu, jest tylko jedna taka krawędź,
 - po której i tak przechodzimy w ramach dodawania
 - nowego wpisu do słownika.
 -
 - Co robić, gdy zmodyfikowany stan jest już
 - elementem automatu?
 - * Modyfikujemy oryginalny stan "w miejscu"
 - * Wyszukujemy go w automacie
 - * Jeśli nie ma, to koniec obliczeń
 - * Jeśli jest, "usuwamy" zmodyfikowany stan i
 -   przekierowujemy krawędzie które wchodziły do
 -   zmodyfikowanego stanu tak, żeby wchodziły do
 -   izomorficznego stanu znalezionego w automacie
 -   (no problem).
 -
 - Zgodnie z opisaną wyżej procedurą w niektórych
 - przypadkach będziemy usuwali stany automatu.
 - Co będziemy robić z takimi stanami?  Możemy je
 - przechowywać na "stosie stanów usuniętych".
 - Czy raczej na stosie "identyfikatorów stanów
 - usuniętych", dzięki któremu będziemy mogli
 - szybko dodawać nowe stany do automatu. 
 - 
 -
 -
 - b) Stan konfluentny
 -
 - Co w sytuacji, gdy stan jest konfluentny?
 - Jedna "instancja" oryginalnego stanu ma zostać
 - usunięta, natomiast pozostałe -- nie.
 - Tak więc powinniśmy utworzyć nowe "pole", w którym
 - zapisany zostanie stan zmodyfikowany.  Oryginalny stan
 - nie zostanie usunięty, jest wiele ścieżek do niego
 - wchodzących. 
 -
 - ALE: Jest możliwe, że zmodyfikowany stan jest już
 - elementem automatu.
 - TEZA: W takiej sytuacji nie ma sensu tworzyć "pola"
 - na nowy stan, ponieważ pole to i tak nie zostanie
 - wykorzystane.  Stan zmodyfikowany zostanie skopiowany
 - do nowego pola, a potem okaże się że zmodyfikowany
 - stan jest już w automacie i nowe pole zostanie
 - usunięte.
 -
 - POMYSŁ: Zamiast kopiować stan do nowego pola, możemy
 - zmodyfikować aktualny stan i sprawdzić czy taki stan
 - znajduje się w automacie.  Przez ten krótki czas:
 - * Nie będzie zgodności pomiędzy stanem a tablicą haszów.
 - * Zawartość stanu będzie nieprawidłowa.
 -
 - Informacje o nowym stanie powinny być dodawane do tablicy
 - haszów tylko wtedy, gdy do automatu dodawany jest rzeczywiście
 - nowy stan.  Powinniśmy podzielić funkcje ze względu na to,
 - czu zachowują spójność pomiędzy tablicą stanów oraz tablicą
 - haszów.
 -
 -
 - Stan aplikacji
 - ==============
 - 
 - Elementy stanu aplikacji:
 -
 - * Tablica (wektor) stanów automatu.  Do każdego stanu
 -   przypisane są wartość oraz zbiór krawędzi wychodzących.
 -   Przez identifykator stanu będziemy rozumieć pozycję
 -   tego stanu w tablicy stanów automatu. 
 -
 - * Tablica asocjacyjna, która pozwala przekształcać stany
 -   (a dokładniej, ich hasze) na identyfikatory stanów.
 -   Warto zauważyć, że operację odwrotną mamy w pewnym
 -   sensie za darmo: znając identyfikator stanu, możemy
 -   szybko wyszukać odpowiedni stan w tablicy stanów automatu,
 -   na podstawie którego możemy już obliczeń wartość hasz.
 -}


module Data.DAWG.Dynamic.Internal
(
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<$))
import           Control.Monad (forM_)
import           Data.Maybe (fromJust)
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


-- | A state of DFA application.
data DFA_State s = DFA_State {
    -- | A vector of DFA states.  A position of a state
    -- in the vector represents its `StateID`.
      stateTab  :: V.MVector s State
    -- | A vector of free state slots.
    , freeTab   :: P.Stack s StateID
    -- | A hash table which is used to translate states
    -- to their corresponding identifiers.
    , stateMap  :: M.Map State StateID }


-- | A DFA reference.
type DFA s = STRef s (DFA_State s)


----------------------------------------------------------------------
-- Primitive operations
--
-- These operations *do not* guarantee consistency between the set of
-- registered states and the hash table used for quick lookup.
----------------------------------------------------------------------


-- | Retrieve state with a given identifier.
getS :: DFA s -> StateID -> ST s State
getS dfa i = do
    us <- stateTab <$> readSTRef dfa
    V.read us i


-- | Set state with a given identifier.
setS :: DFA s -> StateID -> State -> ST s ()
setS dfa i u = do
    us <- stateTab <$> readSTRef dfa
    V.write us i u


-- | Grow DFA with new, empty states.
grow :: DFA s -> ST s ()
grow dfa = do
    dfaState@DFA_State{..} <- readSTRef dfa
    let n = V.length stateTab

    -- Use n+1 to handle empty dfa.
    stateTab' <- V.grow stateTab (n + 1)
    writeSTRef dfa $ dfaState { stateTab = stateTab' }

    forM_ [n .. 2*n] $ \i -> do
        setS dfa i N.empty
        P.push i freeTab


-- | Get the outgoing transition.  Return `Nothin` if there is
-- no transition on a given symbol.
getTrans :: DFA s -> StateID -> Sym -> ST s (Maybe StateID)
getTrans dfa i x = N.getTrans x <$> getS dfa i


-- | Set the outgoing transition.  Use `Nothing` to delete
-- the transition.
setTrans :: DFA s -> StateID -> Sym -> Maybe StateID -> ST s ()
setTrans dfa i x j = do
--     us <- stateTab <$> readSTRef dfa
--     u  <- V.read us i
--     V.write us i (N.setTrans x j u)
    u <- getS dfa i
    setS dfa i $ N.setTrans x j u


-- | Lookup state identifier in a DFA.
lookup :: DFA s -> State -> ST s (Maybe StateID)
lookup dfa u = M.lookup u . stateMap <$> readSTRef dfa


-- | Same as `lookup`, but looks for a state equivalent to a state residing
-- under a given identifier.  Intuitively, it should be the same state ID
-- as the one supplied as argument.  However, if the state is not yet
-- registered in the hash table `stateMap`, the function will return
-- another equivalent state in the automaton, if present, and this is
-- the functionality we use in other, higher-level functions.
lookupID :: DFA s -> StateID -> ST s (Maybe StateID)
lookupID dfa i = getS dfa i >>= lookup dfa


-- | Add new state into the automaton.  The function doesn't
-- check, if the state is already a member of the automaton.
addState_ :: DFA s -> State -> ST s StateID
addState_ dfa u = do
    free <- freeTab <$> readSTRef dfa
    P.pop free >>= \case
        Just i  -> i <$ setS dfa i u
        Nothing -> do
            grow dfa
            fromJust <$> P.pop free


-- | Add new state into the automaton.  First check, if it
-- is not already a member of the automaton.
addState :: DFA s -> State -> ST s StateID
addState dfa u = lookup dfa u >>= \case
    Nothing -> addState_ dfa u
    Just i  -> return i


-- | Create a new branch in a DFA.
branch :: DFA s -> [Sym] -> Val -> ST s StateID
branch dfa (x:xs) y = do
    j <- branch dfa xs y
    addState dfa $ N.state Nothing [(x, j)]


----------------------------------------------------------------------
-- Medium-level interface
----------------------------------------------------------------------


-- -- | Insert a (word, value) pair within a context of
-- -- a confluent state.
-- -- TODO: Handle empty word case.
-- insertConfl :: [Sym] -> Val -> StateID -> DFA s -> ST s StateID
-- insertConfl (x:xs) y i0 dfa = do
-- 
--     -- Store target of the current x-transition.
--     j0 <- getTrans dfa i0 x
-- 
--     -- Determine root state ID of a branch below.
--     j1 <- Just <$> case j0 of
--         -- A child of a confluent state is also a confluent state.
--         Just j  -> insertConfl dfa xs y j
--         Nothing -> branch dfa xs y
--     -- TODO: Stop computation when (j0 == j1)?
-- 
--     -- Add the outgoing transition.  The current state is changed.
--     -- Hash of the state is also changed here and now.
--     setTrans dfa i0 x j1
-- 
--     -- Lookup identical state.
--     i1 <- lookup dfa i0 >>= case of
--         Just i  -> return i
--         Nothing -> copy dfa i0
-- 
--     -- Restore the current state to its original form.
--     setTrans dfa i0 x j0
-- 
--     -- Return the resultant state.
--     return i1


-- -- | Insert a (word, value) pair within a context of
-- -- a non-confluent state.
-- insertNonConfl :: [Sym] -> Val -> StateID -> DFA StateID
-- insertNonConfl (x:xs) y i0 = do
-- 
--     -- Store target of the current x-transition.
--     j0 <- getTrans i0 x
-- 
--     -- Determine root state ID of a branch below.
--     j1 <- case j0 of
--         Just j  -> insert xs y j
--         Nothing -> branch xs y
--     -- TODO: Stop computation when (j0 == j1)?
-- 
--     -- Add the outgoing transition.  The current state is changed.
--     -- Hash of the state is also changed here and now.
--     setTrans i0 x j1
-- 
--     -- Lookup identical state.
--     lookup i0 >>= case of
--         Nothing -> return i0
--         Just i  -> do
--             -- TODO: Should we restore the (x -> j0)
--             -- transition before deletion?
--             delete i0
--             return i
--
--
----------------------------------------------------------------------
-- OLD VERSION
----------------------------------------------------------------------
--
--
-- -- | Insert a (word, value) pair into the automaton.
-- -- Return ID of the new branch.
-- insert :: [Sym] -> Val -> StateID -> DFA StateID
-- insert (x:xs) y i0 = do
-- 
--     -- Clone the state if it is confluent
--     i <- confluent i0 >>= \b -> if b
--         then clone i0
--         else i0
-- 
--     -- { Cloned state is not in the hash table }
-- 
--     -- Determine ID of a branch below
--     -- TODO: You should check if the insert subcall changed the state ID.
--     j <- follow i x >>= \mj -> case mj of
--         Nothing -> branch xs y
--         Just j' -> insert xs y j'
-- 
--     -- Change the outgoing transition
--     -- Q: Should the change be immediately visible
--     -- in the automaton?  In which parts?
--     setTrans i x j
-- 
--     -- Lookup identical state with a different ID and
--     -- merge both states
--     lookupDup i >>= \case _dp of
--         Nothing -> return i
--         Just dp -> merge i j
--     
-- 
-- -- | Retrieve state ID by following a symbol from a source state ID.
-- follow :: StateID -> Sym -> DFA (Maybe StateID)
-- follow = undefined
-- 
-- 
-- -- | Make new branch.
-- branch :: [Sym] -> Val -> DFA StateID
-- branch = undefined
-- 
-- 
-- -- | Is it a counfluent state?  We can check a number of
-- -- ingoing paths to determine this.
-- confluent :: StateID -> DFA Bool
-- confluent = undefined
-- 
-- 
-- -- | Set the outgoing transition.
-- setTrans :: StateID -> Sym -> StateID -> DFA ()
-- setTrans = undefined
-- 
-- 
-- -- | Lookup identical state (from the same equivalence class)
-- -- with a different ID.
-- --
-- --
-- -- Suppose we have a hashtable of (state hash -> stateID) type.
-- -- We want to find a duplicate state.  What do we do?
-- --
-- -- First we identify the hash of the given state.  Then we
-- -- lookup the hash in the hashtable.  What do we find?
-- --
-- -- Well, if the given state ID has been updated in the
-- -- hashtable, at least two stateIDs should be found.
-- -- In this case, we can filter out the ID given as
-- -- argument.  Otherwise, it the hastable has not been
-- -- updated, we don't have to do that, no problem.
-- --
-- lookupDup :: StateID -> DFA (Maybe StateID)
-- lookupDup = undefined
