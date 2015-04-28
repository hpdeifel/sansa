{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, DataKinds, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}

module Data.Units
       ( module X
       , DataSizeDim(..)
       , DataSize
       , Byte(..)

       , Time
       , TimeDim(..)
       , Second(..)
       , Minute(..)
       , Hour(..)
       , showTime

       , DataSpeed
       , DataSpeedDim
       , dataSpeedDim
       , showInBest
       )
       where

import Data.Metrology as X
import Data.Metrology.Show as X ()
import Text.Printf

------------------------------------------------------------
-- Data Size -----------------------------------------------
------------------------------------------------------------

data DataSizeDim = DataSizeDim 

instance Dimension DataSizeDim

data Byte = Byte

instance Show Byte where
  show _ = "B"

instance Unit Byte where
  type BaseUnit Byte = Canonical
  type DimOfUnit Byte = DataSizeDim

type instance DefaultUnitOfDim DataSizeDim = Byte

type DataSize = MkQu_D DataSizeDim

data KiBi = KiBi

kibi :: unit -> KiBi :@ unit
kibi = (KiBi :@)

instance Show KiBi where
  show _ = "Ki"

data MiBi = MiBi

mibi :: unit -> MiBi :@ unit
mibi = (MiBi :@)

instance Show MiBi where
  show _ = "Mi"

data GiBi = GiBi

gibi :: unit -> GiBi :@ unit
gibi = (GiBi :@)

instance Show GiBi where
  show _ = "Gi"

instance UnitPrefix KiBi where
  multiplier _ = 1024

instance UnitPrefix MiBi where
  multiplier _ = 1024 * 1024

instance UnitPrefix GiBi where
  multiplier _ = 1024 * 1024 * 1024

------------------------------------------------------------
-- Time ----------------------------------------------------
------------------------------------------------------------

data TimeDim = TimeDim

instance Dimension TimeDim

data Second = Second

instance Unit Second where
  type BaseUnit Second = Canonical
  type DimOfUnit Second = TimeDim

instance Show Second where
  show _ = "s"

type instance DefaultUnitOfDim TimeDim = Second

data Minute = Minute

instance Unit Minute where
  type BaseUnit Minute = Second
  type DimOfUnit Minute = TimeDim
  conversionRatio _ = 60

instance Show Minute where
  show _ = "m"

data Hour = Hour

instance Unit Hour where
  type BaseUnit Hour = Second
  type DimOfUnit Hour = TimeDim
  conversionRatio _ = 60*60

instance Show Hour where
  show _ = "h"

type Time = MkQu_D TimeDim

showTime :: Time -> String
showTime t = unwords [h', m', s']
  where h = floor $ t # Hour :: Int
        m = floor $ (t |-| (fi h % Hour)) # Minute :: Int
        s = floor $ (t |-| (fi h % Hour) |-| (fi m % Minute)) # Second :: Int

        fi = fromIntegral

        h' = if h == 0 then "" else fmt h "h"
        m' = if h == 0 && m == 0 then "" else fmt m "m"
        s' = fmt s "s"

        fmt :: Int -> String -> String
        fmt = printf "%02d%s"

------------------------------------------------------------
-- Speed ---------------------------------------------------
------------------------------------------------------------

type DataSpeedDim = DataSizeDim :/ TimeDim

dataSpeedDim :: DataSpeedDim
dataSpeedDim = DataSizeDim :/ TimeDim

type DataSpeed = DataSize %/ Time

------------------------------------------------------------
-- Support functions ---------------------------------------
------------------------------------------------------------

data SomeUnit dim = forall unit. (Unit unit, Show unit, ValidDLU (DimFactorsOf dim) DefaultLCSU unit, DimOfUnit unit ~ dim) => SomeUnit unit

instance Show (SomeUnit dim) where
  show x = case x of
    SomeUnit unit -> show unit

class HasBestUnit dim where
  bestUnit :: dim -> MkQu_D dim -> SomeUnit dim

bestDataSize :: DataSize -> SomeUnit DataSizeDim
bestDataSize x
  | x # kibi Byte < 1 = SomeUnit Byte
  | x # mibi Byte < 1 = SomeUnit (kibi Byte)
  | x # gibi Byte < 1 = SomeUnit (mibi Byte)
  | otherwise         = SomeUnit (gibi Byte)

instance HasBestUnit DataSizeDim where
  bestUnit _ = bestDataSize

instance HasBestUnit TimeDim where
  bestUnit _ _ = SomeUnit Second

bestSpeed :: DataSpeed -> SomeUnit DataSpeedDim
bestSpeed x
  | x # kibi Byte :/ Second < 1 = SomeUnit $ Byte :/ Second
  | x # mibi Byte :/ Second < 1 = SomeUnit $ kibi Byte :/ Second
  | x # gibi Byte :/ Second < 1 = SomeUnit $ mibi Byte :/ Second
  | otherwise         = SomeUnit $ gibi Byte :/ Second

-- TODO Generalize

instance HasBestUnit DataSpeedDim where
  bestUnit _ = bestSpeed

showInBest :: (HasBestUnit dim) => (Double -> String) -> dim -> MkQu_D dim -> String
showInBest format dim x = case bestUnit dim x of
  SomeUnit unit -> format (x # unit) ++ " " ++ show unit
