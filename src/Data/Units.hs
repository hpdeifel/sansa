{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, DataKinds, ExistentialQuantification, ConstraintKinds, MultiParamTypeClasses, FlexibleInstances, AllowAmbiguousTypes #-}

module Data.Units
       ( module X
       , DataSizeDim(..)
       , DataSize
       , Byte(..)

       , Time
       , TimeDim(..)
       , Second(..)

       , DataSpeed
       , DataSpeedDim
       , dataSpeedDim
       , showInBest
       )
       where

import Data.Metrology as X
import Data.Metrology.Show as X ()

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

type Time = MkQu_D TimeDim

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
