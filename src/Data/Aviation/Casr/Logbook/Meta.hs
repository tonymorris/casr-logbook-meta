{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Casr.Logbook.Meta(
  AircraftUsageExpense(AircraftUsageExpense)
, AircraftLandingExpense(AircraftLandingExpense)
, AircraftFlightExpense(ExpenseAircraftUsage, ExpenseAircraftLanding)
, SimulatorFlightExpense(SimulatorFlightExpense)
, ExamExpense(ExamExpense)
, BriefingExpense(BriefingExpense)
, Visualisation(Doarama)
, ImageType(Jpg, Png, Gif)
, Image(Image)
, TrackLogType(Gpx, Kml, Kmz, ImageTrackLog)
, TrackLog(TrackLog)
, VideoType(YouTube, Vimeo, Bambuser)
, linkVideoType
, iframeVideoType
, Video(Video)
, TrackLogs(TrackLogs)
, AircraftFlightMeta(AircraftFlightMeta)
, SimulatorFlightMeta(SimulatorFlightMeta)
, ExamMeta(ExamMeta)
, BriefingMeta(BriefingMeta)  
) where

import Control.Lens(makeClassy, makeClassyPrisms, makeWrapped)
import Data.Eq(Eq)
import Data.Int(Int)
import Data.List((++))
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.String(String)
import Prelude(Show)

data AircraftUsageExpense =
  AircraftUsageExpense {
    _aircraftusageexpenseperhour :: Int
  , _aircraftusageexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftUsageExpense

data AircraftLandingExpense =
  AircraftLandingExpense {
    _aircraftlandingexpenseamount :: Int
  , _aircraftlandingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''AircraftLandingExpense

data AircraftFlightExpense =
  ExpenseAircraftUsage AircraftUsageExpense
  | ExpenseAircraftLanding AircraftLandingExpense
  deriving (Eq, Ord, Show)

makeClassyPrisms ''AircraftFlightExpense

data SimulatorFlightExpense =
  SimulatorFlightExpense {
    _simulatorflightexpenseperhour :: Int
  , _simulatorflightexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightExpense

data ExamExpense =
  ExamExpense {
    _examexpenseamount :: Int
  , _examexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''ExamExpense

data BriefingExpense =
  BriefingExpense {
    _briefingexpenseperhour :: Int
  , _briefingexpensename :: String
  } deriving (Eq, Ord, Show)

makeClassy ''BriefingExpense

data Visualisation =
  Doarama {
    _doaramaid :: String
  , _oembedid :: String
  , _doaramaname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Visualisation

data ImageType =
  Jpg
  | Png
  | Gif
  deriving (Eq, Ord, Show)

makeClassyPrisms ''ImageType

data Image =
  Image {
    _imageuri :: String
  , _imagetype :: ImageType
  , _imagesource :: Maybe String
  , _imagename :: Maybe String
  } deriving (Eq, Ord, Show)
  
makeClassy ''Image

data TrackLogType =
  Gpx
  | Kml
  | Kmz
  | ImageTrackLog ImageType
  deriving (Eq, Ord, Show)

makeClassyPrisms ''TrackLogType

data TrackLog =
  TrackLog {
    _trackloguri :: String
  , _tracklogtype :: TrackLogType
  , _tracklogsource :: Maybe String
  , _tracklogname :: Maybe String
  } deriving (Eq, Ord, Show)
  
makeClassy ''TrackLog

data VideoType =
  YouTube
  | Vimeo
  | Bambuser
  deriving (Eq, Ord, Show)
  
makeClassyPrisms ''VideoType

linkVideoType ::
  VideoType
  -> String
  -> String
linkVideoType YouTube u =
  "https://www.youtube.com/watch?v=" ++ u
linkVideoType Vimeo u =
  "https://bambuser.com/v/" ++ u 
linkVideoType Bambuser u =
  "https://vimeo.com/" ++ u

iframeVideoType ::
  VideoType
  -> String
  -> String
iframeVideoType YouTube u =
  "http://www.youtube.com/embed/" ++ u ++ "?autohide=1&amp;cc_load_policy=1&amp;color=white&amp;controls=1&amp;disablekb=0&amp;fs=1&amp;iv_load_policy=0&amp;loop=0&amp;modestbranding=1&amp;rel=0&amp;showinfo=0"
iframeVideoType Vimeo u =
  "https://player.vimeo.com/video/" ++ u
iframeVideoType Bambuser u =
  "https://embed.bambuser.com/broadcast/" ++ u ++ "?chat=1&amp;mute=0"

data Video =
  Video {
    _videouri :: String
  , _videotype :: VideoType
  , _videosource :: Maybe String
  , _videoname :: Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Video

newtype TrackLogs =
  TrackLogs
    [TrackLog]
  deriving (Eq, Ord, Show)

makeClassy ''TrackLogs
makeWrapped ''TrackLogs

data AircraftFlightMeta =
  AircraftFlightMeta {
    _tracklogs :: [TrackLog]
  , _visualisations :: [Visualisation]
  , _images :: [Image]
  , _videos :: [Video]
  , _expenses :: [AircraftFlightExpense]
  } deriving (Eq, Ord, Show)

makeClassy '' AircraftFlightMeta

newtype SimulatorFlightMeta =
  SimulatorFlightMeta
    [SimulatorFlightExpense]
  deriving (Eq, Ord, Show)

makeClassy ''SimulatorFlightMeta
makeWrapped ''SimulatorFlightMeta

newtype ExamMeta =
  ExamMeta
    [ExamExpense]
  deriving (Eq, Ord, Show)

makeClassy ''ExamMeta
makeWrapped ''ExamMeta

newtype BriefingMeta =
  BriefingMeta
    [BriefingExpense]
  deriving (Eq, Ord, Show)

makeClassy ''BriefingMeta
makeWrapped ''BriefingMeta
