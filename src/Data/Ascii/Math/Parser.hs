{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Ascii.Math.Parser
  ( AsciiMathError (..),
    AsciiMathParser,
    parseMathMaybe,
  )
where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text (Text)
import Text.Megaparsec (MonadParsec, ParsecT, parseMaybe)

-- | Errors.
--
-- @since 1.0
data AsciiMathError = AsciiMathError
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Ord
    )

-- | @since 1.0
newtype AsciiMathParser (a :: Type)
  = AsciiMathParser (ParsecT AsciiMathError Text Identity a)
  deriving
    ( -- | @since 1.0
      Functor,
      -- | @since 1.0
      Applicative,
      -- | @since 1.0
      Alternative,
      -- | @since 1.0
      Monad,
      -- @since 1.0
      MonadPlus,
      -- | @since 1.0
      MonadParsec AsciiMathError Text
    )
    via (ParsecT AsciiMathError Text Identity)

-- | @since 1.0
parseMathMaybe ::
  forall (a :: Type).
  AsciiMathParser a ->
  Text ->
  Maybe a
parseMathMaybe (AsciiMathParser comp) = parseMaybe comp
