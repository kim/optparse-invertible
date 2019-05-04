{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Options.Invertible
    ( Parser
    , Parser'
    , Inverse

    , option
    , flag
    , flag'
    , switch
    , argument

    , subparser
    --, command

    , info

    , module Options.Applicative
    )
where

import           Control.Applicative
import           Data.Bool            (bool)
import           Data.Foldable
import           Data.Functor.Compose
import           Data.Option
import           Data.Set             (Set, fromList)
import           Data.Text            (Text, unpack)
import           Options.Applicative  hiding
    ( Parser
    , argument
    , flag
    , flag'
    , info
    , option
    , subparser
    , switch
    )
import qualified Options.Applicative  as Opt


type Inverse a = (a -> Set (Opt Text))

type Parser' a = Parser a a

type Parser a b = Compose Opt.Parser ((,) (Inverse a)) b

instance {-# OVERLAPPING #-}
    Alternative (Compose Opt.Parser ((,) (Inverse a)))
  where
    empty = Compose empty

    (Compose a) <|> (Compose b) = Compose $ a <|> b

    many (Compose c) = Compose $ do
        cs <- many c
        pure $ case cs of
           []    -> (mempty, [])
           (x:_) -> (fst x, map snd cs)

    some (Compose c) = Compose $ do
        cs <- some c
        pure $ case cs of
            []    -> (mempty, [])
            (x:_) -> (fst x, map snd cs)

option
    :: Foldable t
    => Text
    -> (a -> t Text)
    -> ReadM b
    -> Mod OptionFields b
    -> Parser a b
option l unread readm mods = Compose $
    (fromList . map (Opt l) . toList . unread,)
        <$> Opt.option readm (mods <> long (unpack l))

flag
    :: Text
    -> b    -- ^ default
    -> b    -- ^ active
    -> Mod FlagFields b
    -> Parser a b
flag l def act mods = Compose $
    (const [Flag l],)
        <$> Opt.flag def act (mods <> Opt.long (unpack l))

flag'
    :: Text
    -> b
    -> Mod FlagFields b
    -> Parser a b
flag' l act mods = Compose $
    (const [Flag l],)
        <$> Opt.flag' act (mods <> long (unpack l))

switch
    :: Text
    -> (a -> Bool)
    -> Mod FlagFields Bool
    -> Parser a Bool
switch l f mods = Compose $
    (bool mempty [Flag l] . f,)
        <$> Opt.switch (mods <> long (unpack l))

argument
    :: Foldable t
    => (a -> t Text)
    -> ReadM b
    -> Mod ArgumentFields b
    -> Parser a b
argument unread readm mods = Compose $
    (fromList . map Arg . toList . unread,)
        <$> Opt.argument readm mods

subparser :: Mod CommandFields (Inverse a, b) -> Parser a b
subparser = Compose . Opt.subparser

info :: Parser a b -> InfoMod (Inverse a, b) -> ParserInfo (Inverse a, b)
info = Opt.info . getCompose
