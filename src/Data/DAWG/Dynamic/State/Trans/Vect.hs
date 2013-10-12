{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MagicHash #-}


-- | Implementation of a transition map.


module Data.DAWG.Dynamic.State.Trans.Vect
(
-- * Transition map (ST)
  Trans
, empty
, singleton
, null
, lookup
, insert
, delete
, toProd
, overwrite

-- * Transition map (pure)
, Trans'
, printTrans'

-- * Conversion
, thaw
, freeze
, unsafeFreeze
) where


import           Prelude hiding (null, lookup)
import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad (forM_)
import           Control.Monad.ST
import           Pipes
import           Data.Monoid (mconcat)
import           Data.Hashable
import           Data.Bits (shiftR)
import           Data.STRef
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as I

-- Just to get Hashable instance.
import qualified Data.Vector.Unboxed.Base as IB
import qualified Data.Vector.Primitive.Mutable as PM
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Primitive.Types as Prim
import           GHC.Exts (Int(..))
-- import qualified Foreign.Storable as F

import           Data.DAWG.Dynamic.Types


------------------------------------------------------------------
-- Transition map (ST)
------------------------------------------------------------------


-- | An ST version of transition map.
data State s = State
    { symv :: M.MVector s Sym
    , stiv :: M.MVector s StateID
    , size :: {-# UNPACK #-} !Int }


-- | An ST version of transition map.
type Trans s = STRef s (State s)


-- | Empty transition map.
empty :: ST s (Trans s)
empty = do
    st <- State <$> M.new 1 <*> M.new 1 <*> pure 0
    newSTRef st


-- | Single-element transition map.
singleton :: Sym -> StateID -> ST s (Trans s)
singleton x y = do
    st <- State <$> M.replicate 1 x <*> M.replicate 1 y <*> pure 1
    newSTRef st


-- | Is the map empty?
null :: Trans s -> ST s Bool
null t = (==0) . size <$> readSTRef t


-- | Lookup symbol in the map.
lookup :: Sym -> Trans s -> ST s (Maybe StateID)
lookup x t = do
    mk <- index x t
    case mk of
        Just k  -> Just <$> unsafeByIndex k t
        Nothing -> return Nothing


-- | Find index of the given symbol.
index :: Sym -> Trans s -> ST s (Maybe Int)
index x t = do
    State{..} <- readSTRef t
    either Just (const Nothing) <$>
        binarySearch x symv 0 size


-- -- | Get state ID under the given position.
-- byIndex :: Int -> Trans s -> ST s StateID
-- byIndex k t = do
--     State{..} <- readSTRef t
--     M.read stiv k


-- | Get state ID under the given position.
unsafeByIndex :: Int -> Trans s -> ST s StateID
unsafeByIndex k t = do
    State{..} <- readSTRef t
    M.unsafeRead stiv k


-- | Insert element to the map.
insert :: Sym -> StateID -> Trans s -> ST s ()
insert x y t = do
    State{..} <- readSTRef t
    ek <- binarySearch x symv 0 size
    case ek of
        Left k  -> M.unsafeWrite stiv k y
        Right k -> insertOn x y k t
--             let (v'L, v'R) = U.splitAt k v
--             in  U.concat [v'L, U.singleton (x, y), v'R]


-- | Insert new element to the map immediately after the given number
-- of elements.  Or, in other words, place element on the given
-- position, shifting all elements on the right of the position one
-- place further right.
insertOn :: Sym -> StateID -> Int -> Trans s -> ST s ()
insertOn x y k t = do
    st@State{..} <- checkGrow t
    forM_ [size, size-1 .. k+1] $ \i -> do
        M.unsafeWrite symv i =<< M.unsafeRead symv (i-1)
        M.unsafeWrite stiv i =<< M.unsafeRead stiv (i-1)
    M.unsafeWrite symv k x
    M.unsafeWrite stiv k y
    writeSTRef t $ st { size = size + 1 }


-- | Remove element from the map (if it is present there).
delete :: Sym -> Trans s -> ST s ()
delete x t = do
    State{..} <- readSTRef t
    ek <- binarySearch x symv 0 size
    case ek of
        Left k  -> deleteOn k t
        Right _ -> return ()


-- | Delete particular position in the transition map.
deleteOn :: Int -> Trans s -> ST s ()
deleteOn k t = do
    -- TODO: Could do `checkShrink` here?
    st@State{..} <- readSTRef t
    forM_ [k, k+1 .. size-2] $ \i -> do
        M.unsafeWrite symv i =<< M.unsafeRead symv (i+1)
        M.unsafeWrite stiv i =<< M.unsafeRead stiv (i+1)
    writeSTRef t $ st { size = size - 1 }


-- | Translate map into a producer.
toProd :: Trans s -> Producer (Sym, StateID) (ST s) ()
toProd t = do
    State{..} <- lift $ readSTRef t
    forM_ [0 .. size-1] $ \i -> do 
        x <- lift $ M.unsafeRead symv i
        y <- lift $ M.unsafeRead stiv i
        yield (x, y)


-- | Overwrite the contents of the first map with the contents
-- of the second map.
--
-- TODO: perhaps we could make somehow use of the memory allocated
-- within the destination transition map?   On the other hand,
-- cf. the question in the comments of the Internal.setSate
-- function.
overwrite :: Trans s -> Trans s -> ST s ()
overwrite dst src = do
    State{..} <- readSTRef src
    writeSTRef dst =<< State
        <$> M.clone symv
        <*> M.clone stiv
        <*> pure size
-- [*] This version is wrong!  We have to make a new instance of data!
-- overwrite :: Trans s -> Trans s -> ST s ()
-- overwrite dst src = do
--     st <- readSTRef src
--     writeSTRef dst st


-- | Grow the map before `insertOn`.
checkGrow :: Trans s -> ST s (State s)
checkGrow t = do
    st@State{..} <- readSTRef t
    if size < M.length symv
        then return st
        else do 
            symv' <- M.grow symv size
            stiv' <- M.grow stiv size
            return $ st { symv = symv', stiv = stiv' }


------------------------------------------------------------------
-- Transition map (pure)
------------------------------------------------------------------


-- | A pure, immutable version of the transition map.
data Trans' = Trans'
    { symv' :: I.Vector Sym
    , stiv' :: I.Vector StateID
    , size' :: {-# UNPACK #-} !Int }
    deriving (Show)


-- TODO: Optimize implementation?
instance Hashable Trans' where
    hashWithSalt s Trans'{..} = s
        `hashVec` symv'
        `hashVec` stiv'
        -- `hashWithSalt` size'
      where
        -- hashIVec v = I.toList $ I.slice 0 size' v
        hashVec s' v = runST $ do
            IB.MV_Int (PM.MVector i n mba) <- I.unsafeThaw $ I.slice 0 size' v
            Prim.ByteArray ba <- Prim.unsafeFreezeByteArray mba
            let c = I# (Prim.sizeOf# (0 :: Sym))
            return $ hashByteArrayWithSalt ba (i*c) (n*c) s'
            -- return $ I.toList $ I.slice 0 size' v
            


instance Eq Trans' where
    t1 == t2 =
        let size1 = size' t1
            size2 = size' t2
            n     = size1
            symv1 = symv' t1
            symv2 = symv' t2
            stiv1 = stiv' t1
            stiv2 = stiv' t2
        in  and [ size1 == size2
                , I.slice 0 n symv1 == I.slice 0 n symv2
                , I.slice 0 n stiv1 == I.slice 0 n stiv2 ]


instance Ord Trans' where
    compare t1 t2 =
        let size1 = size' t1
            size2 = size' t2
            n     = size1
            symv1 = symv' t1
            symv2 = symv' t2
            stiv1 = stiv' t1
            stiv2 = stiv' t2
        in  mconcat
                [ size1 `compare` size2
                , I.slice 0 n symv1 `compare` I.slice 0 n symv2
                , I.slice 0 n stiv1 `compare` I.slice 0 n stiv2 ]


-- | Print information about the transition map.
printTrans' :: Trans' -> IO ()
printTrans' Trans'{..} = forM_ [0..size'-1] $ \i -> do
    print (symv' I.! i, stiv' I.! i)


------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------


-- | Translate map to its pure version.
freeze :: Trans s -> ST s Trans'
freeze t = do
    State{..} <- readSTRef t
    Trans' 
        <$> I.freeze symv
        <*> I.freeze stiv
        <*> pure size


-- | Translate map to its pure version.
unsafeFreeze :: Trans s -> ST s Trans'
unsafeFreeze t = do
    State{..} <- readSTRef t
    Trans' 
        <$> I.unsafeFreeze symv
        <*> I.unsafeFreeze stiv
        <*> pure size


-- | Translate map to its ST version.
thaw :: Trans' -> ST s (Trans s)
thaw Trans'{..} = do
    newSTRef =<< State
        <$> I.thaw symv'
        <*> I.thaw stiv'
        <*> pure size'

------------------------------------------------------------------
-- Misc
------------------------------------------------------------------


-- | Given a strictly ascending vector, find an index at which the
-- given symbol could be inserted while preserving sortedness.
-- The 'Left' result indicates, that the 'EQ' element has been found,
-- while the 'Right' result means otherwise.  Value of the 'Right'
-- result is in the [0,n] range.
binarySearch :: Sym -> M.MVector s Sym -> Int -> Int -> ST s (Either Int Int)
binarySearch x v =
    loop
  where
    cmpx = compare x
    loop !l !u
        | u <= l    = return (Right l)
        | otherwise = do
            let k = (u + l) `shiftR` 1
            y <- M.unsafeRead v k
            case cmpx y of
                GT -> loop (k+1) u
                EQ -> return (Left k)
                LT -> loop l k
{-# INLINE binarySearch #-}
