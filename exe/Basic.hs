{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens          (to, view)
import           Data.Bifunctor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Generics.Sum     (_Ctor)
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe
import           Data.Text             (Text, pack)
import           GHC.Generics          (Generic)
import           Options.Invertible

data Example = Example
    { optFoo :: Int
    , optBar :: Bool
    , optBaz :: Maybe Bool
    , optXyz :: String
    , optLst :: [Int]
    , optNe  :: NonEmpty Int
    , optDef :: String
    , optCmd :: Cmd
    } deriving (Show, Generic)

data Cmd
    = Start StartOptions
    | Stop  StopOptions
    | Pause
    deriving (Show, Generic)

data StartOptions = StartOptions
    { optStartAfterSecs :: Word
    } deriving (Show, Generic)

data StopOptions = StopOptions
    { optStopGracefully :: Bool
    } deriving (Show, Generic)

exampleParser :: Parser' Example
exampleParser = do
    optFoo <-
        (  option "foo" (Identity . shown . optFoo) auto (help "The foo")
       <|> option "lol" (Identity . shown . optFoo) auto (help "The LOL")
        )
    optBar <-
        flag "bar" optBar False True (help "Das Bar")
    optBaz <-
        optional $
            option "baz"
                   (maybeToList . fmap shown . optBaz)
                   auto
                   (help "Bazzz")
    optXyz <-
        argument (Identity . pack . optXyz) str (metavar "FILE")
    optLst <-
        many $ option "lst" (map shown . optLst) auto (help "many ints")
    optNe  <-
        fmap NE.fromList . some $
            option "ne" (fmap shown . optNe) auto (help "some ints")
    optDef <-
        option "def" (Identity . pack . optDef) str (value "default value")
    optCmd <-
         Compose $ first (. optCmd) <$> getCompose cmdParser

    pure Example {..}

cmdParser :: Parser' Cmd
cmdParser = hsubparser $ start <> stop <> pause
  where
    start =
        command "start"
                (\f -> view (_Ctor @"Start" . to f))
                (Start <$> startOptionsParser)
                (progDesc "Start it")

    stop =
        command "stop"
                (\f -> view (_Ctor @"Stop" . to f))
                (Stop <$> stopOptionsParser)
                (progDesc "Stop it")

    pause =
        command "pause"
                (\f -> view (_Ctor @"Pause" . to f))
                (pure Pause)
                (progDesc "Pause it")

startOptionsParser :: Parser' StartOptions
startOptionsParser = StartOptions
    <$> option "after" (Identity . shown . optStartAfterSecs) auto
            (help "Start after this many seconds")

stopOptionsParser :: Parser' StopOptions
stopOptionsParser = StopOptions
    <$> switch "gracefully" optStopGracefully mempty

main :: IO ()
main = do
    (inv, ex) <- execParser (info exampleParser briefDesc)
    print (ex :: Example)
    print $ inv ex
    print $ inv ex { optCmd = Stop (StopOptions True) }

--------------------------------------------------------------------------------

shown :: Show a => a -> Text
shown = pack . show
