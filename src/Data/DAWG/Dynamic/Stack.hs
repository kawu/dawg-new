{-# LANGUAGE RecordWildCards #-}


module Data.DAWG.Dynamic.Stack
( Stack
, empty
, push
, pop
) where


import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as U


-- | TODO: Perhaps we should use unboxed tuple here?
data State s a = State
    { body  :: !(U.MVector s a)
    , size  :: {-# UNPACK #-} !Int }


-- | A stack of unboxed values.
type Stack s a = STRef s (State s a)


-- | A new, empty stack.
empty :: U.Unbox a => ST s (Stack s a)
empty = do
    st <- State <$> U.new 10 <*> pure 0
    newSTRef st


-- | Push value on top of the stack.
push :: U.Unbox a => a -> Stack s a -> ST s ()
push x ref = do
    st@State{..} <- checkGrow ref
    U.write body size x
    writeSTRef ref $ st { size = size + 1 }


-- | Grow stack before push. 
checkGrow :: U.Unbox a => Stack s a -> ST s (State s a)
checkGrow ref = do
    st@State{..} <- readSTRef ref
    if size < U.length body
        then return st
        else do 
            body' <- U.grow body size
            return $ st { body = body' }


-- | Pop value from the stack.
pop :: U.Unbox a => Stack s a -> ST s (Maybe a)
pop ref = do
    st@State{..} <- readSTRef ref
    if size <= 0
        then return Nothing
        else do
            writeSTRef ref $ st { size = size - 1 }
            Just <$> U.read body (size - 1)
