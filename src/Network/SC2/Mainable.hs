{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.SC2.Mainable
       ( module Options.Applicative
       , (<>)
       , OptParseable(..)
       , Mainable(..)
       , withMain
       ) where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Proxy

class OptParseable a where
    optParse :: Parser a

class Mainable a where
    type MainOpt a
    mainParse :: Proxy a -> Parser (MainOpt a)
    withMainArgs :: MainOpt a -> a -> IO ()

withMain :: (Mainable a) => InfoMod (MainOpt a) -> a -> IO ()
withMain im a = do
  args <- execParser $ info (mainParse (pure a) <**> helper) im
  withMainArgs args a

instance (OptParseable a, Mainable b) => Mainable (a -> b) where
    type MainOpt (a -> b) = (a, MainOpt b)
    mainParse p = (,) <$> optParse <*> mainParse (p <*> Proxy)
    withMainArgs (a, b) f = withMainArgs b (f a)

instance Mainable (IO ()) where
    type MainOpt (IO ()) = ()
    mainParse _ = pure ()
    withMainArgs _ = id
