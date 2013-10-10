{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


-- | Implementation of a transition map build on top of the "M.Map" container.


module Data.DAWG.State.EdgeMap.Map
( EdgeMap
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<*>))
import           Data.STRef
import qualified Data.Map as M

import           Data.DAWG.Types
import qualified Data.DAWG.State.EdgeMap as E


-- | A vector of distinct key/value pairs strictly ascending with respect
-- to key values.
newtype EdgeMap s = EdgeMap
    { edgeMapRef :: STRef s (M.Map Sym StateID)
    } deriving (Show, Eq, Ord)


instance C.Trans Trans where
    empty = EdgeMap <$> newSTRef M.empty
    {-# INLINE empty #-}

    singleton x y = EdgeMap <$> newSTRef (M.singleton x y)
    {-# INLINE singleton #-}

    null EdgeMap{..} = M.null <$> readSTRef edgeMapRef
    {-# INLINE null #-}

    lookup x EdgeMap{..} = M.lookup x <$> readSTRef edgeMapRef
    {-# INLINE lookup #-}

    insert x y EdgeMap{..} = modifySTRef edgeMapRef $! M.insert x y
    {-# INLINE insert #-}

    delete x EdgeMap{..} = modifySTRef edgeMapRef $! M.delete x
    {-# INLINE delete #-}

    toProd EdgeMap{..} = do
        xs <- lift $ M.toList <$> readSTRef edgeMapRef
        mapM_ yield xs
    {-# INLINE toProd #-}


--     index x = M.lookupIndex x . unTrans
--     {-# INLINE index #-}
-- 
--     byIndex i (Trans m) =
-- 	let n = M.size m
--         in  if i >= 0 && i < n
--                 then Just (M.elemAt i m)
--                 else Nothing
--     {-# INLINE byIndex #-}
-- 
-- 
--     fromList = Trans . M.fromList
--     {-# INLINE fromList #-}
-- 
--     toList = M.toList . unTrans
--     {-# INLINE toList #-}
