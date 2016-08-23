{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook.Meta.BriefingExpense(
  BriefingExpense(BriefingExpense)
) where

import Control.Lens(makeClassy)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data BriefingExpense =
  BriefingExpense {
    _briefingexpenseperhour :: Int
  , _briefingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''BriefingExpense
