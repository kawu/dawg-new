{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


-- | Implementation of a transition map build on top of the "M.Map" container.


module Data.DAWG.Dynamic.State.EdgeMap.Map
(
-- * Transition map (ST)
  EdgeMap
, empty
, singleton
, null
, lookup
, insert
, delete
, toProd
, overwrite

-- * Transition map (pure)
, EdgeMap'
, printEdgeMap'

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
newtype EdgeMap s = EdgeMap
    { edgeMapRef :: STRef s (M.Map Sym StateID) }


-- | Empty transition map.
empty :: ST s (EdgeMap s)
empty = EdgeMap <$> newSTRef M.empty


-- | Single-element transition map.
singleton :: Sym -> StateID -> ST s (EdgeMap s)
singleton x y = EdgeMap <$> newSTRef (M.singleton x y)


-- | Is the map empty?
null :: EdgeMap s -> ST s Bool
null EdgeMap{..} = M.null <$> readSTRef edgeMapRef


-- | Lookup symbol in the map.
lookup :: Sym -> EdgeMap s -> ST s (Maybe StateID)
lookup x EdgeMap{..} = M.lookup x <$> readSTRef edgeMapRef


-- | Insert element to the map.
insert :: Sym -> StateID -> EdgeMap s -> ST s ()
insert x y EdgeMap{..} = modifySTRef edgeMapRef $! M.insert x y


-- | Remove element from the map.
delete :: Sym -> EdgeMap s -> ST s ()
delete x EdgeMap{..} = modifySTRef edgeMapRef $! M.delete x


-- | Translate map into a producer.
toProd :: EdgeMap s -> Producer (Sym, StateID) (ST s) ()
toProd EdgeMap{..} = do
    xs <- lift $ M.toList <$> readSTRef edgeMapRef
    mapM_ yield xs


-- | Overwrite the contents of the first map with the contents
-- of the second map.
overwrite :: EdgeMap s -> EdgeMap s -> ST s ()
overwrite dst src = writeSTRef (edgeMapRef dst)
                =<< readSTRef  (edgeMapRef src)


------------------------------------------------------------------
-- Transition map (pure)
------------------------------------------------------------------


-- | A pure, immutable version of the transition map.
newtype EdgeMap' = EdgeMap' (M.Map Sym StateID)
    deriving (Show, Eq, Ord)


-- | Print information about the transition map.
printEdgeMap' :: EdgeMap' -> IO ()
printEdgeMap' (EdgeMap' edgeMap) = mapM_ print $ M.toList edgeMap


------------------------------------------------------------------
-- Conversion
------------------------------------------------------------------


-- | Translate map to its pure version.
freeze :: EdgeMap s -> ST s EdgeMap'
freeze EdgeMap{..} = EdgeMap' <$> readSTRef edgeMapRef


-- | Translate map to its pure version.
unsafeFreeze :: EdgeMap s -> ST s EdgeMap'
unsafeFreeze = freeze


-- | Translate map to its ST version.
thaw :: EdgeMap' -> ST s (EdgeMap s)
thaw (EdgeMap' edgeMap) = EdgeMap <$> newSTRef edgeMap
