{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bifunctor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.List.NonEmpty    (NonEmpty)
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe
import           Data.Option           (Opt (Arg))
import           Data.Set              (singleton)
import           Data.Text             (Text, pack)
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
    } deriving Show

data Cmd
    = Start StartOptions
    | Stop  StopOptions
    deriving Show

data StartOptions = StartOptions
    { optStartAfterSecs :: Word
    } deriving Show

data StopOptions = StopOptions
    { optStopGracefully :: Bool
    } deriving Show

exampleParser :: Parser' Example
exampleParser = do
    optFoo <-
        (  option "foo" (Identity . shown . optFoo) auto (help "The foo")
       <|> option "lol" (Identity . shown . optFoo) auto (help "The LOL")
        )
    optBar <-
        flag "bar" False True (help "Das Bar")
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
         Compose $ first ((. optCmd)) <$> getCompose cmdParser

    pure Example {..}

cmdParser :: Parser' Cmd
cmdParser = subparser $ start <> stop
  where
    start       = command "start" startInfo
    startInfo   = info startParser (progDesc "Start it")
    startParser = Compose $
        let
            sp       = Start <$> startOptionsParser
            inv' inv = \case
                Start so -> singleton (Arg "start") <> inv so
                _        -> mempty
         in
            first inv' <$> getCompose sp

    stop       = command "stop" stopInfo
    stopInfo   = info stopParser (progDesc "Stop it")
    stopParser = Compose $
        let
            sp = Stop <$> stopOptionsParser
            inv' inv = \case
                Stop so -> singleton (Arg "stop") <> inv so
                _       -> mempty
         in
            first inv' <$> getCompose sp

startOptionsParser :: Parser' StartOptions
startOptionsParser = StartOptions
    <$> option "after" (Identity . shown . optStartAfterSecs) auto
            (help "Start after this many seconds")

stopOptionsParser :: Parser' StopOptions
stopOptionsParser = StopOptions
    <$> switch "gracefully" optStopGracefully mempty

main :: IO ()
main = do
    (inv, ex) <- execParser pinfo
    print (ex :: Example)
    print $ inv ex
    print $ inv ex { optFoo = 666 }
  where
    pinfo :: ParserInfo (Inverse Example, Example)
    pinfo = info exampleParser briefDesc

--------------------------------------------------------------------------------

shown :: Show a => a -> Text
shown = pack . show
