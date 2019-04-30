{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}

module Options.Invertible
    ( Parser'
    , Inverse (..)

    , option
    , flag
    , flag'
    , switch
    , argument

    , module Options.Applicative
    )
where

import           Data.Functor.Compose
import           Data.Option
import           Data.Set             (Set, fromList)
import           Data.Text            (Text, unpack)
import           Options.Applicative  hiding
    ( argument
    , flag
    , flag'
    , option
    , switch
    )
import qualified Options.Applicative  as Opt


newtype Inverse a = Inverse { fromInverse :: (a -> Set (Opt Text)) }
    deriving (Semigroup, Monoid)

type Parser' a b = Compose Parser ((,) (Inverse a)) b

option
    :: Text
    -> (a -> [Text])
    -> ReadM b
    -> Mod OptionFields b
    -> Parser' a b
option l unread readm mods = Compose $
    (Inverse $ fromList . map (Opt l) . unread,)
        <$> Opt.option readm (mods <> long (unpack l))

flag
    :: Text
    -> b    -- ^ default
    -> b    -- ^ active
    -> Mod FlagFields b
    -> Parser' a b
flag l def act mods = Compose $
    (Inverse $ const [Flag l],)
        <$> Opt.flag def act (mods <> long (unpack l))

flag'
    :: Text
    -> b
    -> Mod FlagFields b
    -> Parser' a b
flag' l act mods = Compose $
    (Inverse $ const [Flag l],)
        <$> Opt.flag' act (mods <> long (unpack l))

switch
    :: Text
    -> Mod FlagFields Bool
    -> Parser' a Bool
switch l mods = Compose $
    (Inverse $ const [Flag l],)
        <$> Opt.switch (mods <> long (unpack l))

argument
    :: (a -> [Text])
    -> ReadM b
    -> Mod ArgumentFields b
    -> Parser' a b
argument unread readm mods = Compose $
    (Inverse $ fromList . map Arg . unread,)
        <$> Opt.argument readm mods
