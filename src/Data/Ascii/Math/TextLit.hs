{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ascii.Math.TextLit
  ( MathTextLitType (..),
    MathTextLit,
    parseMathTextLit,
  )
where

import Control.Applicative ((<|>))
import Data.Ascii.Math.Parser (AsciiMathParser)
import Data.Char (isAscii, isPrint, isSpace)
import Data.Text (Text)
import Text.Megaparsec
  ( between,
    chunk,
    satisfy,
    skipMany,
    takeWhileP,
    try,
  )

data MathTextLitType
  = -- | @since 1.0
    QuoteTL
  | -- | @since 1.0
    TextTL
  | -- | @since 1.0
    MathbfTL
  | -- | @since 1.0
    MathbbTL
  | -- | @since 1.0
    MathcalTL
  | -- | @since 1.0
    MathttTL
  | -- | @since 1.0
    MathfrakTL
  | -- | @since 1.0
    MathsfTL
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | @since 1.0
data MathTextLit = MathTextLit MathTextLitType Text
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | @since 1.0
parseMathTextLit :: AsciiMathParser MathTextLit
parseMathTextLit =
  try (MathTextLit QuoteTL <$> parseQuoted)
    <|> try (MathTextLit TextTL <$> parseText)
    <|> try (MathTextLit MathbfTL <$> (chunkSpace "mathbf" *> parseQuoted))
    <|> try (MathTextLit MathbbTL <$> (chunkSpace "mathbb" *> parseQuoted))
    <|> try (MathTextLit MathcalTL <$> (chunkSpace "mathcal" *> parseQuoted))
    <|> try (MathTextLit MathttTL <$> (chunkSpace "mathtt" *> parseQuoted))
    <|> try (MathTextLit MathfrakTL <$> (chunkSpace "mathfrak" *> parseQuoted))
    <|> try (MathTextLit MathsfTL <$> (chunkSpace "mathsf" *> parseQuoted))
    <|> try (MathTextLit MathbbTL <$> (chunkSpace "bbb" *> parseQuoted))
    <|> try (MathTextLit MathbfTL <$> (chunkSpace "bb" *> parseQuoted))
    <|> try (MathTextLit MathcalTL <$> (chunkSpace "cc" *> parseQuoted))
    <|> try (MathTextLit MathttTL <$> (chunkSpace "tt" *> parseQuoted))
    <|> try (MathTextLit MathfrakTL <$> (chunkSpace "fr" *> parseQuoted))
    <|> try (MathTextLit MathsfTL <$> (chunkSpace "sf" *> parseQuoted))
  where
    parseQuoted :: AsciiMathParser Text
    parseQuoted =
      between
        (chunk "\"")
        (chunk "\"")
        (takeWhileP (Just "quoted literal contents") isQuotedAcceptable)
    parseText :: AsciiMathParser Text
    parseText =
      chunkSpace "text"
        *> between
          (chunk "(")
          (chunk ")")
          (takeWhileP (Just "text literal contents") isTextAcceptable)
    chunkSpace :: Text -> AsciiMathParser ()
    chunkSpace lit = chunk lit *> skipMany (satisfy isPrintableWhitespace)
    isQuotedAcceptable :: Char -> Bool
    isQuotedAcceptable c = isAscii c && isPrint c && (c /= '"')
    isTextAcceptable :: Char -> Bool
    isTextAcceptable c = isAscii c && isPrint c && (c /= ')')
    isPrintableWhitespace :: Char -> Bool
    isPrintableWhitespace c = isAscii c && isSpace c
