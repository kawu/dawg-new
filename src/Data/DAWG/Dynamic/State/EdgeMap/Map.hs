{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


-- | Implementation of a transition map build on top of the "M.Map" container.


module Data.DAWG.Dynamic.State.EdgeMap.Map
( EdgeMap
, EdgeMapI
, unsafeFreeze
, unsafeThaw
) where


import           Prelude hiding (lookup)
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Map as M
import           Pipes

import           Data.DAWG.Dynamic.Types
import qualified Data.DAWG.Dynamic.State.EdgeMap as E


-- | A pure, immutable version of the transition map.
newtype EdgeMapI = EdgeMapI (M.Map Sym StateID)
    deriving (Eq, Ord)


-- | An ST version of transition map.
newtype EdgeMap s = EdgeMap
    {edgeMapRef :: STRef s (M.Map Sym StateID)}


-- | Translate map to its pure version.
unsafeFreeze :: EdgeMap s -> ST s EdgeMapI
unsafeFreeze EdgeMap{..} = EdgeMapI <$> readSTRef edgeMapRef


-- | Translate map to its ST version.
unsafeThaw :: EdgeMapI -> ST s (EdgeMap s)
unsafeThaw (EdgeMapI edgeMap) = EdgeMap <$> newSTRef edgeMap


instance E.EdgeMap EdgeMap where
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
