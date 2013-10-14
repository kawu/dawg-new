-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE DeriveGeneric #-}

-- {-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}


-- | An automaton state.


module Data.DAWG.Dynamic.ByteState
(
-- * State (ST)
--   State (..)
-- , empty
-- , null
-- , mkValue
-- , mkEdge
-- , getValue
-- , setValue
-- , getTrans
-- , setTrans
-- , edgeProd
-- , overwrite
-- -- * State (imutable)
-- , State' (..)
-- , freeze
-- , unsafeFreeze
-- , thaw
-- , printState'
) where


import           Prelude hiding (null)
import           GHC.Exts
import           Control.Monad.ST
import qualified GHC.ST as I


-- import qualified Data.Primitive.Types as Prim

-- import           Control.Applicative ((<$>), (<*>))
-- import           Pipes
-- import           Data.STRef
-- import           Data.Hashable
-- -- import qualified Data.Map as M

import           Data.DAWG.Dynamic.Types

-- -- Choice of the low-level transition representation.
-- import qualified Data.DAWG.Dynamic.State.Trans.Vect as E


---------------------------------------------------
-- Maybe encoding/decoding
---------------------------------------------------


decodeMaybe :: Word -> Maybe Int
decodeMaybe = undefined


---------------------------------------------------
-- Word encoding/decoding
---------------------------------------------------


decodeWord# :: Word# -> State# s -> (# State# s, Word# #)
decodeWord# = undefined


---------------------------------------------------
-- ST version of State
---------------------------------------------------


data State s = State (MutableByteArray# s)


-- | An empty state.
-- TODO: Use initial size of one machine word.
empty :: ST s (State s)
empty = I.ST $ \s# -> case newByteArray# 8# s# of
    (# s'#, arr# #) -> (# s'#, State arr# #)
    -- where n# = Prim.sizeOf# (0 :: Val)
    -- where n# = SIZEOF_WORD
{-# INLINE empty #-}


-- | Get a state value.
getValue :: State s -> ST s (Maybe Val)
-- getValue :: State s -> ST s Val
getValue (State arr#) = fmap decodeMaybe $ I.ST $ \s0# ->
    case readWordArray# arr# 0# s0# of
      (# s1#, x# #) -> case decodeWord# x# s1#
        of (# s2#, y# #) -> (# s2#, W# y# #)



-- -- | Is the state empty?
-- null :: State s -> ST s Bool
-- null State{..} = do
--     v <- readSTRef valueRef
--     e <- E.null edgeMap
--     return $ v == Nothing && e 


-- -- | A new, only-value state.
-- mkValue :: Val -> ST s (State s)
-- mkValue v = State <$> newSTRef (Just v) <*> E.empty
-- 
-- 
-- -- | A new, only-edge state.
-- mkEdge :: Sym -> StateID -> ST s (State s)
-- mkEdge x y = State <$> newSTRef Nothing <*> E.singleton x y
-- 
-- 
-- -- | Get a state value.
-- getValue :: State s -> ST s (Maybe Val)
-- getValue State{..} = readSTRef valueRef
-- 
-- 
-- -- | Set a state value.
-- setValue :: Maybe Val -> State s -> ST s ()
-- setValue v State{..} = writeSTRef valueRef v
-- 
-- 
-- -- | Get the outgoing transition on a given symbol or `Nothing`.
-- getTrans :: Sym -> State s -> ST s (Maybe StateID)
-- getTrans x State{..} = E.lookup x edgeMap
-- 
-- 
-- -- | Set the outgoing transition on a given symbol.
-- -- Use `Nothing` to delete the transition.
-- setTrans :: Sym -> Maybe StateID -> State s -> ST s ()
-- setTrans x mj State{..} = case mj of
--     Nothing -> E.delete x edgeMap
--     Just j  -> E.insert x j edgeMap
-- 
-- 
-- -- | A producer of all state transitions.
-- edgeProd :: State s -> Producer (Sym, StateID) (ST s) ()
-- edgeProd State{..} = E.toProd edgeMap
-- 
-- 
-- -- | Overwrite the contents of the first state with the contents
-- -- of the second state.
-- overwrite :: State s -> State s -> ST s ()
-- overwrite dst src = do
--     -- State'{..} <- unsafeFreeze src
--     writeSTRef (valueRef dst) =<< readSTRef (valueRef src)
--     E.overwrite (edgeMap dst) (edgeMap src)
-- 
-- 
-- ---------------------------------------------------
-- -- Pure version of a state
-- ---------------------------------------------------
-- 
-- 
-- -- | A pure version of a state.
-- data State' = State' {
--     -- | A (maybe) value kept in the state.
--       value     :: Maybe Val
--     -- | A map of outgoing edges.
--     , edgeMap'  :: E.Trans'
--     } deriving (Show, Eq, Ord)
-- 
-- 
-- instance Hashable State' where
--     hashWithSalt s State'{..} = s
--         `hashWithSalt` value
--         `hashWithSalt` edgeMap'
-- 
-- 
-- -- | Translate state to its pure version.
-- unsafeFreeze :: State s -> ST s State'
-- unsafeFreeze State{..} = State'
--     <$> readSTRef valueRef
--     <*> E.unsafeFreeze edgeMap
-- 
-- 
-- -- | Translate state to its pure version.
-- freeze :: State s -> ST s State'
-- freeze State{..} = State'
--     <$> readSTRef valueRef
--     <*> E.freeze edgeMap
-- 
-- 
-- -- | Translate state to its pure version.
-- thaw :: State' -> ST s (State s)
-- thaw State'{..} = State <$> newSTRef value <*> E.thaw edgeMap'
-- 
-- 
-- -- | A helper state printing function.
-- printState' :: State' -> IO ()
-- printState' State'{..} = do
--     putStr "value:\t" >> print value
--     E.printTrans' edgeMap'
