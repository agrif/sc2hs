{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Network.SC2.Convert where

import Network.SC2.Types
import qualified Proto.S2clientprotocol.Common as C
import qualified Proto.S2clientprotocol.Sc2api as A

convertRace :: Race' a -> C.Race
convertRace Terran = C.Terran
convertRace Zerg = C.Zerg
convertRace Protoss = C.Protoss
convertRace (Random _) = C.Random

convertRaceBack :: C.Race -> a -> Race' a
convertRaceBack C.Terran = const Terran
convertRaceBack C.Zerg = const Zerg
convertRaceBack C.Protoss = const Protoss
convertRaceBack C.Random = Random

convertPlayer :: Player (Race' a) -> A.PlayerSetup
convertPlayer Observer = A.PlayerSetup (Just A.Observer) Nothing Nothing
convertPlayer (Participant r) = A.PlayerSetup (Just A.Participant) (Just (convertRace r)) Nothing
convertPlayer (Computer r d) = A.PlayerSetup (Just A.Computer) (Just (convertRace r)) (Just d)

class ConvertProto a where
  type Unproto a
  convertTo :: Unproto a -> a
  convertFrom :: a -> Maybe (Unproto a)

instance ConvertProto C.PointI where
  type Unproto C.PointI = (Int, Int)
  convertTo (x, y) = C.PointI (Just (fromIntegral x)) (Just (fromIntegral y))
  convertFrom (C.PointI x y) = (,) <$> (fromIntegral <$> x) <*> (fromIntegral <$> y)

instance ConvertProto C.RectangleI where
  type Unproto C.RectangleI = ((Int, Int), (Int, Int))
  convertTo (a, b) = C.RectangleI (Just (convertTo a)) (Just (convertTo b))
  convertFrom (C.RectangleI a b) = (,) <$> (convertFrom =<< a) <*> (convertFrom =<< b)

instance ConvertProto C.Point2D where
  type Unproto C.Point2D = (Float, Float)
  convertTo (x, y) = C.Point2D (Just x) (Just y)
  convertFrom (C.Point2D x y) = (,) <$> x <*> y

instance ConvertProto C.Size2DI where
  type Unproto C.Size2DI = (Int, Int)
  convertTo (x, y) = C.Size2DI (Just (fromIntegral x)) (Just (fromIntegral y))
  convertFrom (C.Size2DI x y) = (,) <$> (fromIntegral <$> x) <*> (fromIntegral <$> y)

instance ConvertProto C.ImageData where
  type Unproto C.ImageData = ImageData
  convertTo (ImageData bits size dat) = C.ImageData (Just (fromIntegral bits)) (Just (convertTo size)) (Just dat)
  convertFrom (C.ImageData bits size dat) = ImageData <$> (fromIntegral <$> bits) <*> (convertFrom =<< size) <*> dat
