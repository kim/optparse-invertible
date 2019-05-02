{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}

module Options.Invertible
    ( Parser
    , Inverse

    , inverse

    , option
    , flag
    , flag'
    , switch
    , argument

    , info

    , module Options.Applicative
    )
where

import           Control.Applicative
import           Data.Coerce          (coerce)
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
    , switch
    )
import qualified Options.Applicative  as Opt


newtype Inverse a = Inverse ((a -> Set (Opt Text)))
    deriving (Semigroup, Monoid)

inverse :: Inverse a -> a -> Set (Opt Text)
inverse = coerce

newtype Parser a b = Parser (Compose Opt.Parser ((,) (Inverse a)) b)
    deriving (Functor, Applicative)

instance Alternative (Parser a) where
    empty = Parser $
        Compose empty

    a <|> b = Parser $
        coerce a <|> coerce b

    many (Parser (Compose c)) = Parser . Compose $ do
        cs <- many c
        pure $ case cs of
           []    -> (mempty, [])
           (x:_) -> (fst x, map snd cs)

    some (Parser (Compose c)) = Parser . Compose $ do
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
option l unread readm mods = Parser . Compose $
    (Inverse $ fromList . map (Opt l) . toList . unread,)
        <$> Opt.option readm (mods <> long (unpack l))

flag
    :: Text
    -> b    -- ^ default
    -> b    -- ^ active
    -> Mod FlagFields b
    -> Parser a b
flag l def act mods = Parser . Compose $
    (Inverse $ const [Flag l],)
        <$> Opt.flag def act (mods <> Opt.long (unpack l))

flag'
    :: Text
    -> b
    -> Mod FlagFields b
    -> Parser a b
flag' l act mods = Parser . Compose $
    (Inverse $ const [Flag l],)
        <$> Opt.flag' act (mods <> long (unpack l))

switch
    :: Text
    -> Mod FlagFields Bool
    -> Parser a Bool
switch l mods = Parser . Compose $
    (Inverse $ const [Flag l],)
        <$> Opt.switch (mods <> long (unpack l))

argument
    :: Foldable t
    => (a -> t Text)
    -> ReadM b
    -> Mod ArgumentFields b
    -> Parser a b
argument unread readm mods = Parser . Compose $
    (Inverse $ fromList . map Arg . toList . unread,)
        <$> Opt.argument readm mods

info :: Parser a b -> InfoMod (Inverse a, b) -> ParserInfo (Inverse a, b)
info = Opt.info . coerce
