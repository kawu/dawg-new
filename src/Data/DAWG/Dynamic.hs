module Data.DAWG.Dynamic
( DFA (..)
, empty
, insert
, lookup
, printDFA
) where


import           Prelude hiding (lookup)
import           Control.Monad.ST

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic.Internal as I


-- A Minimal, acyclic, deterministic, finite-state automaton.
data DFA s = DFA
    { body  :: I.DFA s
    , root  :: StateID }


-- | An empty DFA.
empty :: ST s (DFA s)
empty = do
    (dfa, i) <- I.empty
    return $ DFA dfa i


-- | Insert a (word, value) pair into the DFA.
insert :: DFA s -> [Sym] -> Val -> ST s ()
insert dfa xs y = I.insertRoot (body dfa) xs y (root dfa)


-- | Lookup a word in the automaton.
lookup :: DFA s -> [Sym] -> ST s (Maybe Val)
lookup dfa xs = I.lookup (body dfa) xs (root dfa)


-- | A helper DFA printing function.
printDFA :: DFA RealWorld -> IO ()
printDFA dfa = I.printDFA $ body dfa
