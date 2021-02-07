{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Ascii.Math where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Exts (IsList (fromList, toList))
import Text.Ascii (AsciiText)
import qualified Text.Ascii as Ascii
import Text.Ascii.Char (AsciiChar)
import Text.Megaparsec
  ( MonadParsec,
    ParsecT,
    Stream
      ( Token,
        Tokens,
        chunkLength,
        chunkToTokens,
        take1_,
        takeN_,
        takeWhile_,
        tokenToChunk,
        tokensToChunk
      ),
  )

newtype Ascii = Ascii AsciiText

instance Stream Ascii where
  type Token Ascii = AsciiChar
  type Tokens Ascii = AsciiText
  {-# INLINEABLE tokenToChunk #-}
  tokenToChunk _ = Ascii.singleton
  {-# INLINEABLE tokensToChunk #-}
  tokensToChunk _ = fromList
  {-# INLINEABLE chunkToTokens #-}
  chunkToTokens _ = toList
  {-# INLINEABLE chunkLength #-}
  chunkLength _ = Ascii.length
  {-# INLINEABLE take1_ #-}
  take1_ = coerce Ascii.uncons
  {-# INLINEABLE takeN_ #-}
  takeN_ n s@(Ascii raw)
    | n <= 0 = Just (Ascii.empty, s)
    | Ascii.length raw == 0 = Nothing
    | otherwise = Just . coerce Ascii.splitAt n $ s
  {-# INLINEABLE takeWhile_ #-}
  takeWhile_ = coerce Ascii.span

newtype AsciiParser (a :: Type) = AsciiParser (ParsecT Void Ascii Identity a)
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadPlus,
      MonadParsec Void Ascii
    )
    via (ParsecT Void Ascii Identity)
