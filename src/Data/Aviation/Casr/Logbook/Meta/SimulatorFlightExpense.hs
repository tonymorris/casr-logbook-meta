{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.SimulatorFlightExpense(
  SimulatorFlightExpense(SimulatorFlightExpense)
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data SimulatorFlightExpense =
  SimulatorFlightExpense {
    _simulatorflightexpenseperhour :: Int
  , _simulatorflightexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightExpense
