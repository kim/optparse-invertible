{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Functor.Compose  (getCompose)
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Text             (Text, pack)
import           Options.Invertible

data Example = Example
    { optFoo :: Int
    , optBar :: Bool
    , optBaz :: Maybe Bool
    , optXyz :: String
    , optLst :: [Int]
    , optDef :: String
    } deriving Show


exampleParser :: Parser Example Example
exampleParser = Example
    <$> (  option "foo" (Identity . shown . optFoo) auto (help "The foo")
       <|> option "lol" (Identity . shown . optFoo) auto (help "The LOL")
        )
    <*> flag "bar" False True (help "Das Bar")
    <*> (optional $
            option "baz"
                   (maybeToList . fmap shown . optBaz)
                   auto
                   (help "Bazzz"))
    <*> argument (Identity . pack . optXyz) str (metavar "FILE")
    <*> (some $ option "lst" (map shown . optLst) auto (help "some ints"))
    <*> option "def" (Identity . pack . optDef) str (value "default value")
  where
    shown :: Show a => a -> Text
    shown = pack . show

main :: IO ()
main = do
    (unparsed, ex) <- execParser pinfo
    print (ex :: Example)
    print $ fromInverse unparsed ex
    print $ fromInverse unparsed ex { optFoo = 666 }
  where
    pinfo = info (getCompose $ fromParser exampleParser) briefDesc
