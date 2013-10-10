{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


-- | Implementation of a transition map build on top of the "M.Map" container.


module Data.DAWG.Dynamic.State.Trans.Map
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
import           Control.Applicative ((<$>))
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Map as M
import           Pipes

import           Data.DAWG.Dynamic.Types


------------------------------------------------------------------
-- Transition map (ST)
------------------------------------------------------------------


-- | An ST version of transition map.
newtype Trans s = Trans
    { edgeMapRef :: STRef s (M.Map Sym StateID) }


-- | Empty transition map.
empty :: ST s (Trans s)
empty = Trans <$> newSTRef M.empty


-- | Single-element transition map.
singleton :: Sym -> StateID -> ST s (Trans s)
singleton x y = Trans <$> newSTRef (M.singleton x y)


-- | Is the map empty?
null :: Trans s -> ST s Bool
null Trans{..} = M.null <$> readSTRef edgeMapRef


-- | Lookup symbol in the map.
lookup :: Sym -> Trans s -> ST s (Maybe StateID)
lookup x Trans{..} = M.lookup x <$> readSTRef edgeMapRef


-- | Insert element to the map.
insert :: Sym -> StateID -> Trans s -> ST s ()
insert x y Trans{..} = modifySTRef edgeMapRef $! M.insert x y


-- | Remove element from the map.
delete :: Sym -> Trans s -> ST s ()
delete x Trans{..} = modifySTRef edgeMapRef $! M.delete x


-- | Translate map into a producer.
toProd :: Trans s -> Producer (Sym, StateID) (ST s) ()
toProd Trans{..} = do
    xs <- lift $ M.toList <$> readSTRef edgeMapRef
    mapM_ yield xs


-- | Overwrite the contents of the first map with the contents
-- of the second map.
overwrite :: Trans s -> Trans s -> ST s ()
overwrite dst src = writeSTRef (edgeMapRef dst)
                =<< readSTRef  (edgeMapRef src)


------------------------------------------------------------------
-- Transition map (pure)
------------------------------------------------------------------


-- | A pure, immutable version of the transition map.
newtype Trans' = Trans' (M.Map Sym StateID)
    deriving (Show, Eq, Ord)


-- | Print information about the transition map.
printTrans' :: Trans' -> IO ()
printTrans' (Trans' edgeMap) = mapM_ print $ M.toList edgeMap


------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------


-- | Translate map to its pure version.
freeze :: Trans s -> ST s Trans'
freeze Trans{..} = Trans' <$> readSTRef edgeMapRef


-- | Translate map to its pure version.
unsafeFreeze :: Trans s -> ST s Trans'
unsafeFreeze = freeze


-- | Translate map to its ST version.
thaw :: Trans' -> ST s (Trans s)
thaw (Trans' edgeMap) = Trans <$> newSTRef edgeMap
