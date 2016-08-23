{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.AircraftUsageExpense(
  AircraftUsageExpense(AircraftUsageExpense)
, HasAircraftUsageExpense(..)
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data AircraftUsageExpense =
  AircraftUsageExpense {
    _aircraftusageexpenseperhour :: Int
  , _aircraftusageexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftUsageExpense
