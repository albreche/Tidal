{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Sound.Tidal.Page (module Sound.Tidal.Page) where


import Sound.Tidal.Pattern hiding (empty)
import Sound.Tidal.ID
import Sound.Tidal.Params 
import Sound.Tidal.Show
import Sound.Tidal.Core


import GHC.Generics (Generic)
import Data.List (intercalate, delete, findIndex, (\\))
import qualified Data.Map.Strict     as Map
import Data.Maybe

-- Slice
data Slice a = Slice {pats:: [a]}
  deriving (Generic, Functor)

instance Applicative Slice  where
    pure v = Slice {pats = [v]}
    (<*>) s1 s2 =  applySliceToSlice s1 s2

applySliceToSlice :: Slice (a -> b) -> Slice a -> Slice b 
applySliceToSlice s1 s2 = Slice $ applyFs (pats s1) (pats s2)
  where
    applyFs (f:fs) vs = (map f vs) ++ (applyFs fs vs)
    applyFs [] vs = []

-- Page
type Slices a = Map.Map ID (Slice a)

data Page a = Page {segs :: Slices a}
  deriving (Generic,Functor)

empty = Page Map.empty

instance Applicative Page where
    pure v = insertSlice (lOrbit 1) (pure v) empty
    (<*>) l1 l2 =  applyPageToPage l1 l2

applyPageToPage :: Page (a -> b) -> Page a -> Page b 
applyPageToPage l1 l2 = Page p
  where
    vs k = Map.lookup k $ segs l1 
    applyS (Just s) s2 = Just (s <*> s2)
    applyS Nothing _ = Nothing
    p = Map.mapMaybeWithKey (\k2 s2 -> applyS (vs k2) s2) $ segs l2 

-- utilities
insertSlice :: ID -> Slice a -> Page a -> Page a
insertSlice id os op = Page np 
  where np = Map.insertWith (\s1 s2 -> Slice { pats = (pats s1) ++ (pats s2) }) id os (segs op)

lOrbit :: Integer -> ID
lOrbit n = fromInteger n

-- Show
instance (Show a) => Show (Slice a) where
    show s = show $ pats s

instance (Show a) => Show (Page a) where
    show l = show $ segs l

insertPat id pat op = insertSlice id (Slice {pats = [pat]}) op

-- insertDPat id pat op = Page np
--   where np = Map.M




     






