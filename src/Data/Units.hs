{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, DataKinds, ExistentialQuantification, ConstraintKinds #-}

module Data.Units
       ( module X
       , DataSize
       , Byte(..)
       , showInBest
       )
       where

import Data.Metrology as X
import Data.Metrology.Show as X ()

data DataSizeDim

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

data SomeUnit dim n = forall unit. (Unit unit, Show unit, ValidDLU dim DefaultLCSU unit, Fractional n) => SomeUnit unit

instance Show (SomeUnit dim n) where
  show x = case x of
    SomeUnit unit -> show unit

bestDataSize :: DataSize -> SomeUnit (DimFactorsOf DataSizeDim) Double
bestDataSize x
  | x # kibi Byte < 1 = SomeUnit Byte
  | x # mibi Byte < 1 = SomeUnit (kibi Byte)
  | x # gibi Byte < 1 = SomeUnit (mibi Byte)
  | otherwise         = SomeUnit (gibi Byte)

showInBest :: (Double -> String) -> DataSize -> String
showInBest format x = case bestDataSize x of
  SomeUnit unit -> format (x # unit) ++ " " ++ show unit
