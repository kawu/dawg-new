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
 -
 -
 - Notatki
 - =======
 -
 - Na etapie obliczeń musimy aktualizować (m.in.) dwa ważne elementy
 - stanu: liczby wchodzących do poszczególnych stanów ścieżek oraz 
 - mapę haszów.
 -
 -}


module Data.DAWG.Dynamic.Internal
( DFA_State (..)
, DFA
, insert
, insertRoot
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<$))
import           Control.Monad (forM_)
import           Data.Int (Int32)
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
      stateVect :: V.MVector s State
    -- | A number of ingoing paths (size of the left language)
    -- for each state in the automaton.
    , ingoVect  :: U.MVector s Int32
    -- | A stack of free state slots.
    , freeStack :: P.Stack s StateID
    -- | A hash table which is used to translate states
    -- to their corresponding identifiers.
    , stateMap  :: M.Map State StateID }


-- | A DFA reference.
type DFA s = STRef s (DFA_State s)


----------------------------------------------------------------------
-- Low-level operations
----------------------------------------------------------------------


-- | Grow DFA with new, empty states.
grow :: DFA s -> ST s ()
grow dfa = do
    dfaState@DFA_State{..} <- readSTRef dfa
    let n = V.length stateVect

    -- Use n+1 to handle empty dfa.
    stateVect' <- V.grow stateVect (n + 1)
    ingoVect'  <- U.grow ingoVect  (n + 1)
    writeSTRef dfa $ dfaState
        { stateVect = stateVect'
        , ingoVect  = ingoVect' }

    forM_ [n .. 2*n] $ \i -> do
        P.push i freeStack
        -- TODO: Check, if it is not slower when
        -- using uninitialized values.
        -- setState dfa i N.empty
        -- setI dfa i 0


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


-- | Incerement the number of ingoing paths for a given state.
-- TODO: Implement and use modifyIngo function?
incIngo :: DFA s -> StateID -> ST s ()
incIngo dfa i = do
    v <- ingoVect <$> readSTRef dfa
    x <- U.read v i
    U.write v i (x + 1)


-- | Get the outgoing transition.  Return `Nothin` if there is
-- no transition on a given symbol.
getTrans :: DFA s -> StateID -> Sym -> ST s (Maybe StateID)
getTrans dfa i x = N.getTrans x <$> getState dfa i


-- | Set the outgoing transition.  Use `Nothing` to delete
-- the transition.
-- TODO: Implement modifyTrans and use it here.
setTrans :: DFA s -> StateID -> Sym -> Maybe StateID -> ST s ()
setTrans dfa i x j = do
    u <- getState dfa i
    setState dfa i $ N.setTrans x j u


-- | Get a state value.
getValue :: DFA s -> StateID -> ST s (Maybe Val)
getValue dfa i = N.value <$> getState dfa i


-- | Set a state value.
-- TODO: Implement modifyValue and use it here.
setValue :: DFA s -> StateID -> Maybe Val -> ST s ()
setValue dfa i y = do
    u <- getState dfa i
    setState dfa i $ N.setValue y u


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
lookupID dfa i = getState dfa i >>= lookup dfa


-- | Add new state into the automaton.  The function assumes,
-- that the state is not a member of the automaton.
_add :: DFA s -> State -> ST s StateID
_add dfa u = do
    dfaState@DFA_State{..} <- readSTRef dfa
    i <- P.pop freeStack >>= \case
        Just i  -> return i
        Nothing -> do
            grow dfa
            fromJust <$> P.pop freeStack
    setState dfa i u
    setIngo  dfa i 1
    let stateMap' = M.insert u i stateMap
    writeSTRef dfa $ dfaState { stateMap = stateMap' }
    return i


-- | Add new state into the automaton.  First check, if it
-- is not already a member of the automaton.
--
-- We assume, that every time a new state is added, it is added
-- within a context of a new path.  Therefore, if the state is
-- already a member of the automaton, we increase the number of
-- ingoing paths to this state.
add :: DFA s -> State -> ST s StateID
add dfa u = lookup dfa u >>= \case
    Nothing -> _add dfa u
    Just i  -> i <$ incIngo dfa i


-- | Remove state from the automaton.
remove :: DFA s -> StateID -> ST s ()
remove dfa i = do
    dfaState@DFA_State{..} <- readSTRef dfa
    P.push i freeStack
    stateMap' <- flip M.delete stateMap <$> getState dfa i
    writeSTRef dfa $ dfaState { stateMap = stateMap' }


-- | Create a new branch in a DFA.
branch :: DFA s -> [Sym] -> Val -> ST s StateID
branch dfa (x:xs) y = do
    j <- branch dfa xs y
    add dfa $ N.state Nothing [(x, j)]
branch dfa [] y = do
    add dfa $ N.state (Just y) []


-- | Copy state to a new slot and set the number of
-- ingoing paths to 1 in the new slot.
copy :: DFA s -> StateID -> ST s StateID
copy dfa i = do
    j <- _add dfa =<< getState dfa i
    setIngo dfa j 1
    return j


----------------------------------------------------------------------
-- Medium-level interface
----------------------------------------------------------------------


-- TODO: The functions below will probably "crash" when the element
-- beeing inserted is already element of the DFA.  Or if we change
-- value of existing state.  Check it.


-- | Insert a (word, value) pair within a context of a state.
insert :: DFA s -> [Sym] -> Val -> StateID -> ST s StateID
insert dfa xs y i = do
    ingoNum <- getIngo dfa i
    let doInsert = if ingoNum > 1
            then insertConfl
            else insertNonConfl
    doInsert dfa xs y i


-- | Insert a (word, value) pair within a context of a confluent state.
-- TODO: Cut off computation when (j0 == j1).
insertConfl :: DFA s -> [Sym] -> Val -> StateID -> ST s StateID

-- CASE: Non-empty path.
insertConfl dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- Just <$> case j0 of
        -- A child of a confluent state is also a confluent state.
        Just j  -> insertConfl dfa xs y j
        Nothing -> branch dfa xs y
    setTrans dfa i0 x j1
    i1 <- lookupID dfa i0 >>= \case
        Just i  -> i <$ incIngo dfa i
        Nothing -> copy dfa i0
    setTrans dfa i0 x j0
    return i1

-- CASE: Empty path.
insertConfl dfa [] y1 i0 = do
    y0 <- getValue dfa i0
    setValue dfa i0 $ Just y1
    i1 <- lookupID dfa i0 >>= \case
        Just i  -> i <$ incIngo dfa i
        Nothing -> copy dfa i0
    setValue dfa i0 y0
    return i1


-- | Insert a (word, value) pair within a context of a non-confluent state.
-- TODO: Cut off computation when (j0 == j1)?
insertNonConfl :: DFA s -> [Sym] -> Val -> StateID -> ST s StateID

-- CASE: Non-empty path.
insertNonConfl dfa (x:xs) y i0 = do
    j0 <- getTrans dfa i0 x
    j1 <- Just <$> case j0 of
        Just j  -> insert dfa xs y j
        Nothing -> branch dfa xs y
    setTrans dfa i0 x j1
    lookupID dfa i0 >>= \case
        -- Before and after, the number of ingoing paths is equal to 1.
        Nothing -> return i0
        Just i  -> do
            remove dfa i0
            i <$ incIngo dfa i

-- CASE: Empty path.
insertNonConfl dfa [] y1 i0 = do
    y0 <- getValue dfa i0
    setValue dfa i0 $ Just y1
    i1 <- lookupID dfa i0 >>= \case
        Just i  -> i <$ incIngo dfa i
        Nothing -> copy dfa i0
    setValue dfa i0 y0
    return i1


-- | Insert a (word, value) pair under the assumption, that
-- a state ID represents a DFA root.  In particular, ID
-- of the root doen't change.
insertRoot :: DFA s -> [Sym] -> Val -> StateID -> ST s ()
insertRoot dfa [] y i = setValue dfa i $ Just y
insertRoot dfa (x:xs) y i = do
    j0 <- getTrans dfa i x
    j1 <- Just <$> case j0 of
        Just j  -> insert dfa xs y j
        Nothing -> branch dfa xs y
    setTrans dfa i x j1
