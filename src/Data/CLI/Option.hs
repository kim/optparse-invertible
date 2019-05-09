{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.CLI.Option
    ( Cmd  (..)
    , Opts (..)
    , nil
    , Opt  (..)
    , Var
    , Sh   (..)
    , Op   (..)

    , Eval
    , EvalError
    , runEval

    , evalCmd
    , evalOpt
    , evalSh
    , evalShInt
    , evalArith
    )
where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Proxy                 (Proxy (..))
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Build
import qualified Data.Text.Lazy.Builder.Int as Build
import qualified Data.Text.Read             as Read
import           Data.Typeable              (TypeRep, Typeable, typeOf, typeRep)
import           Data.Void
import           Formatting.Buildable       (Buildable (..))


data Cmd a = Cmd FilePath a
    deriving (Eq, Ord, Show, Read, Functor)

instance Buildable a => Buildable (Cmd a) where
    build (Cmd exe opts) = Build.fromString exe <> " " <> build opts

data Opts a b where
    Nil  ::               Opts a b
    (:&) :: Opt a -> b -> Opts a b

infixr 7 :&

nil :: Opts Void Void
nil = Nil

deriving instance (Show a, Show b) => Show (Opts a b)

deriving instance Functor     (Opts a)
deriving instance Foldable    (Opts a)
deriving instance Traversable (Opts a)

instance Bifunctor Opts where
    bimap f g (Opt  t x :& xs) = Opt  t (f x) :& g xs
    bimap _ g (Flag t   :& xs) = Flag t       :& g xs
    bimap _ g (Arg  t   :& xs) = Arg  t       :& g xs
    bimap _ _ Nil              = Nil

instance Bifoldable Opts where
    bifoldMap f g (Opt  _ x :& xs) = f x <> g xs
    bifoldMap _ g (_        :& xs) = g xs
    bifoldMap _ _ Nil              = mempty

instance Bitraversable Opts where
    bitraverse f g (Opt  t x :& xs) = (\y ys -> Opt t y :& ys) <$> f x <*> g xs
    bitraverse _ g (Flag t   :& xs) = (Flag t :&) <$> g xs
    bitraverse _ g (Arg  t   :& xs) = (Arg  t :&) <$> g xs
    bitraverse _ _ Nil              = pure Nil

instance (Buildable a, Buildable b) => Buildable (Opts a b) where
    build = \case
        Nil    -> mempty
        x :& y -> Build.fromLazyText
                . LText.stripEnd     -- 'Nil' causes a trailing whitespace
                . Build.toLazyText
                $ build x <> " " <> build y

data Opt a where
    Flag :: Text      -> Opt a
    Arg  :: Text      -> Opt a
    Opt  :: Text -> a -> Opt a

deriving instance Eq   a => Eq   (Opt a)
deriving instance Ord  a => Ord  (Opt a)
deriving instance Show a => Show (Opt a)
deriving instance Read a => Read (Opt a)

deriving instance Functor Opt

instance Buildable a => Buildable (Opt a) where
    build = \case
        Flag f   -> "--" <> Build.fromText f
        Opt  n v -> "--" <> Build.fromText n <> "=" <> build v
        Arg  a   -> Build.fromText a

newtype Var = Var Text
    deriving (Eq, Ord, Show, Read)

instance IsString Var where
    fromString = Var . fromString

instance Buildable Var where
    build (Var x) = Build.fromText x

data Sh a where
    ShNum  :: Int  -> Sh Int
    ShStr  :: Text -> Sh Text
    ShVar  :: Var  -> Sh Var
    ShEval :: Sh a -> Sh (Sh a)

    ShCmd  :: Typeable a => Cmd a -> Sh (Cmd a)

    ShArith
        :: ( Show      a
           , Show      b
           , Buildable a
           , Buildable b
           )
        => Op
        -> Sh a
        -> Sh b
        -> Sh Int

deriving instance Show a => Show (Sh a)

instance Buildable a => Buildable (Sh a) where
    build = \case
        ShNum   x      -> Build.decimal x
        ShStr   x      -> Build.fromText x
        ShVar   x      -> "${" <> build x <> "}"
        ShCmd   cmd    -> build cmd
        ShEval  sh     -> "$(" <> build sh <> ")"
        ShArith op a b -> "$((" <> build a <> build op <> build b <> "))"


data Op = Add | Sub | Mul | Div
    deriving (Eq, Ord, Enum, Show, Read)

instance IsString Op where
    fromString = \case
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div
        x   -> error $ "Unkown arithmetic operator: " <> x

instance Buildable Op where
    build = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"


class CanEval a where
    eval :: Opt a -> Eval (Opt Text)

instance CanEval Text where
    eval = pure

instance CanEval (Sh a) where
    eval = evalOpt

instance CanEval Void where
    eval _ = throwError Absurd

class CanEvalF a where
    evalF :: CanEval b => Opts b a -> Eval [Opt Text]

instance CanEvalF Void where
    evalF = \case
        Nil    -> pure []
        x :& _ -> pure <$> eval x

instance (CanEval a, CanEvalF b) => CanEvalF (Opts a b) where
    evalF = \case
        Nil    -> pure []
        x :& y -> (:) <$> eval x <*> evalF y


data EvalEnv = EvalEnv
    { eeVars :: Var -> Maybe Text
    , eeCmds :: forall a. Cmd a -> Maybe Text
    }

data EvalError
    = UnknownVariable Var
    | UnknownCommand  TypeRep
    | CastError       TypeRep TypeRep
    | Absurd
    deriving Show

type Eval a = ExceptT EvalError (Reader EvalEnv) a

runEval
    :: (Var -> Maybe Text)
    -> (forall x. Cmd x -> Maybe Text)
    -> Eval a
    -> Either EvalError a
runEval vs cs = flip runReader (EvalEnv vs cs) . runExceptT

evalCmd
    :: (CanEval a, CanEvalF b)
    => Cmd (Opts a b)
    -> Eval (Cmd [Opt Text])
evalCmd (Cmd exe opts) = Cmd exe <$> evalF opts

evalOpt :: Opt (Sh a) -> Eval (Opt Text)
evalOpt = \case
    Flag x   -> pure $ Flag x
    Arg  x   -> pure $ Arg  x
    Opt  k v -> evalSh v >>= \case
        ShStr x -> pure $ Opt k x

evalSh :: Sh a -> Eval (Sh Text)
evalSh = \case
    ShVar   x      -> ShStr <$> lookupVar x
    ShCmd   x      -> ShStr <$> runCmd    x
    ShEval  x      -> evalSh x
    ShArith op a b ->
        evalSh . ShNum =<<
            evalArith op <$> evalShInt a <*> evalShInt b
    ShNum   x      -> pure . ShStr $ Text.pack (show x)
    ShStr   x      -> pure $ ShStr x
  where
    lookupVar :: Var -> Eval Text
    lookupVar x = do
        vars <- asks eeVars
        note (UnknownVariable x) $ vars x

    runCmd :: Typeable x => Cmd x -> Eval Text
    runCmd x = do
        cmds <- asks eeCmds
        note (UnknownCommand (typeOf x)) $ cmds x

evalShInt :: Sh a -> Eval Int
evalShInt sh = evalSh sh >>= cast >>= int
  where
    int :: Sh Int -> Eval Int
    int = \case
        ShNum   x      -> pure x
        ShArith op a b -> evalArith op <$> evalShInt a <*> evalShInt b

    cast :: Sh Text -> Eval (Sh Int)
    cast (ShStr x) =
        case Read.signed Read.decimal x of
            Right (i, "") -> pure $ ShNum i
            _             -> throwError $ CastError tyFrom tyTo
      where
        tyFrom = typeRep (Proxy @(Sh Text))
        tyTo   = typeRep (Proxy @Int)

evalArith :: Op -> Int -> Int -> Int
evalArith Add = (+)
evalArith Sub = (-)
evalArith Mul = (*)
evalArith Div = div

--------------------------------------------------------------------------------

note :: MonadError e m => e -> Maybe a -> m a
note _ (Just a) = pure a
note e Nothing  = throwError e
