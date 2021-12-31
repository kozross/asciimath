{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ascii.Math.TextLit
  ( MathTextLitType (..),
    MathTextLit,
    textLitType,
    textTL,
    _TextTL,
    quoteTL,
    _QuoteTL,
    mathbfTL,
    _MathbfTL,
    mathbbTL,
    _MathbbTL,
    mathcalTL,
    _MathcalTL,
    mathttTL,
    _MathttTL,
    mathfrakTL,
    _MathfrakTL,
    mathsfTL,
    _MathsfTL,
    parseMathTextLit,
  )
where

import Control.Applicative ((<|>))
import Data.Ascii.Math.Parser (AsciiMathParser)
import Data.Char (isPrint, isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Optics.Prism (Prism', prism')
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

textLitType :: MathTextLit -> MathTextLitType
textLitType (MathTextLit typ _) = typ

-- | @since 1.0
data MathTextLitError
  = -- | @since 1.0
    TextDelimiterFound
  | -- | @since 1.0
    QuoteDelimiterFound
  | -- | @since 1.0
    NonPrintableCharacter Char
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | @since 1.0
_TextTL :: Prism' Text MathTextLit
_TextTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . textTL)

-- | @since 1.0
textTL :: Text -> Either MathTextLitError MathTextLit
textTL t = case Text.find go t of
  Nothing -> pure . MathTextLit TextTL $ t
  Just ')' -> Left TextDelimiterFound
  Just c -> Left . NonPrintableCharacter $ c
  where
    go :: Char -> Bool
    go c = (not . isPrint) c || c == ')'

-- | @since 1.0
_QuoteTL :: Prism' Text MathTextLit
_QuoteTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . quoteTL)

-- | @since 1.0
quoteTL :: Text -> Either MathTextLitError MathTextLit
quoteTL = mkTL QuoteTL

-- | @since 1.0
_MathbfTL :: Prism' Text MathTextLit
_MathbfTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . mathbfTL)

-- | @since 1.0
mathbfTL :: Text -> Either MathTextLitError MathTextLit
mathbfTL = mkTL MathbfTL

-- | @since 1.0
_MathbbTL :: Prism' Text MathTextLit
_MathbbTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . mathbbTL)

-- | @since 1.0
mathbbTL :: Text -> Either MathTextLitError MathTextLit
mathbbTL = mkTL MathbbTL

-- | @since 1.0
_MathcalTL :: Prism' Text MathTextLit
_MathcalTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . mathcalTL)

-- | @since 1.0
mathcalTL :: Text -> Either MathTextLitError MathTextLit
mathcalTL = mkTL MathcalTL

-- | @since 1.0
_MathttTL :: Prism' Text MathTextLit
_MathttTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . mathttTL)

-- | @since 1.0
mathttTL :: Text -> Either MathTextLitError MathTextLit
mathttTL = mkTL MathttTL

-- | @since 1.0
_MathfrakTL :: Prism' Text MathTextLit
_MathfrakTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . mathfrakTL)

-- | @since 1.0
mathfrakTL :: Text -> Either MathTextLitError MathTextLit
mathfrakTL = mkTL MathfrakTL

-- | @since 1.0
_MathsfTL :: Prism' Text MathTextLit
_MathsfTL = prism' (\(MathTextLit _ t) -> t) (either (const Nothing) pure . mathsfTL)

-- | @since 1.0
mathsfTL :: Text -> Either MathTextLitError MathTextLit
mathsfTL = mkTL MathsfTL

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
    isQuotedAcceptable c = isPrint c && (c /= '"')
    isTextAcceptable :: Char -> Bool
    isTextAcceptable c = isPrint c && (c /= ')')
    isPrintableWhitespace :: Char -> Bool
    isPrintableWhitespace c = isPrint c && isSpace c

-- Helpers

mkTL :: MathTextLitType -> Text -> Either MathTextLitError MathTextLit
mkTL typ t = case Text.find go t of
  Nothing -> pure . MathTextLit typ $ t
  Just '"' -> Left QuoteDelimiterFound
  Just c -> Left . NonPrintableCharacter $ c
  where
    go :: Char -> Bool
    go c = (not . isPrint) c || c == '"'
