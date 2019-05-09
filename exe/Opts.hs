{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.CLI.Option
import           Data.Text

vars :: Var -> Maybe Text
vars = \case
    "PORT" -> pure "42"
    _      -> Nothing

cmds :: Cmd a -> Maybe Text
cmds = \case
    Cmd "hostname" _ -> pure "leboeuf"
    _                -> Nothing

main :: IO ()
main =
    let
        cmd = Cmd "/bin/oscoin"
            $ Opt "host" (ShEval (ShCmd @Text (Cmd "hostname" mempty)))
           :& Opt "port" (ShArith Add (ShVar "PORT") (ShNum 1))
           :& nil
     in
        print . runEval vars cmds $ evalCmd cmd
