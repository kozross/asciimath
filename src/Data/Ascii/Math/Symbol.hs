{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ascii.Math.Symbol
  ( MathSymbol (..),
    _Greek,
    _Arrow,
    _Logic,
    _Operation,
    _Relation,
    _Misc,
    _Char,
    _Function,
    _Number,
    parseMathSymbol,
    GreekLetter (..),
    MathArrow (..),
    LogicSymbol (..),
    MathOperation (..),
    RelationSymbol (..),
    MiscSymbol (..),
    FunctionSymbol (..),
    AsciiNumber (..),
    AsciiDigit (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad.Combinators.NonEmpty (some)
import Data.Ascii.Math.Parser (AsciiMathParser)
import Data.Char (isPrint, isSpace)
import Data.Functor (($>))
import Data.List (sortOn)
import Data.Monoid (Alt (Alt, getAlt))
import Data.Ord (Down (Down))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEVector
import Optics.Prism (Prism', prism')
import Text.Megaparsec (chunk, optional, satisfy, try)

-- | Single symbols.
--
-- @since 1.0
data MathSymbol
  = GreekS GreekLetter
  | ArrowS MathArrow
  | LogicS LogicSymbol
  | OperationS MathOperation
  | RelationS RelationSymbol
  | MiscS MiscSymbol
  | CharS Char
  | FunctionS FunctionSymbol
  | NumberS AsciiNumber
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | @since 1.0
_Greek :: Prism' MathSymbol GreekLetter
_Greek = prism' GreekS (\case GreekS gl -> pure gl; _ -> empty)

-- | @since 1.0
_Arrow :: Prism' MathSymbol MathArrow
_Arrow = prism' ArrowS (\case ArrowS arr -> pure arr; _ -> empty)

-- | @since 1.0
_Logic :: Prism' MathSymbol LogicSymbol
_Logic = prism' LogicS (\case LogicS ls -> pure ls; _ -> empty)

-- | @since 1.0
_Operation :: Prism' MathSymbol MathOperation
_Operation = prism' OperationS (\case OperationS mo -> pure mo; _ -> empty)

-- | @since 1.0
_Relation :: Prism' MathSymbol RelationSymbol
_Relation = prism' RelationS (\case RelationS r -> pure r; _ -> empty)

-- | @since 1.0
_Misc :: Prism' MathSymbol MiscSymbol
_Misc = prism' MiscS (\case MiscS m -> pure m; _ -> empty)

-- | @since 1.0
_Char :: Prism' MathSymbol Char
_Char = prism' CharS (\case CharS c -> pure c; _ -> empty)

-- | @since 1.0
_Function :: Prism' MathSymbol FunctionSymbol
_Function = prism' FunctionS (\case FunctionS f -> pure f; _ -> empty)

-- | @since 1.0
_Number :: Prism' MathSymbol AsciiNumber
_Number = prism' NumberS (\case NumberS n -> pure n; _ -> empty)

-- | A number.
--
-- = Note
--
-- Numbers are treated strictly as sequences of digits; thus, we will retain
-- /all/ digits given to us, not just significant ones.
--
-- @since 1.0
data AsciiNumber
  = AsciiNumber (NonEmptyVector AsciiDigit) (Maybe (NonEmptyVector AsciiDigit))
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | A digit.
--
-- @since 1.0
data AsciiDigit
  = -- | @since 1.0
    DZero
  | -- | @since 1.0
    DOne
  | -- | @since 1.0
    DTwo
  | -- | @since 1.0
    DThree
  | -- | @since 1.0
    DFour
  | -- | @since 1.0
    DFive
  | -- | @since 1.0
    DSix
  | -- | @since 1.0
    DSeven
  | -- | @since 1.0
    DEight
  | -- | @since 1.0
    DNine
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Predefined functions.
--
-- @since 1.0
data FunctionSymbol
  = -- | @since 1.0
    SinF
  | -- | @since 1.0
    CosF
  | -- | @since 1.0
    TanF
  | -- | @since 1.0
    SecF
  | -- | @since 1.0
    CscF
  | -- | @since 1.0
    CotF
  | -- | @since 1.0
    ArcsinF
  | -- | @since 1.0
    ArccosF
  | -- | @since 1.0
    ArctanF
  | -- | @since 1.0
    SinhF
  | -- | @since 1.0
    CoshF
  | -- | @since 1.0
    TanhF
  | -- | @since 1.0
    SechF
  | -- | @since 1.0
    CschF
  | -- | @since 1.0
    CothF
  | -- | @since 1.0
    ExpF
  | -- | @since 1.0
    LogF
  | -- | @since 1.0
    LnF
  | -- | @since 1.0
    DetF
  | -- | @since 1.0
    DimF
  | -- | @since 1.0
    ModF
  | -- | @since 1.0
    GcdF
  | -- | @since 1.0
    LcmF
  | -- | @since 1.0
    LubF
  | -- | @since 1.0
    GlbF
  | -- | @since 1.0
    MinF
  | -- | @since 1.0
    MaxF
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Other symbols.
--
-- @since 1.0
data MiscSymbol
  = -- | @since 1.0
    MiscInt
  | -- | @since 1.0
    MiscOInt
  | -- | @since 1.0
    MiscDel
  | -- | @since 1.0
    MiscGrad
  | -- | @since 1.0
    MiscPM
  | -- | @since 1.0
    MiscEmptyset
  | -- | @since 1.0
    MiscInfinity
  | -- | @since 1.0
    MiscAleph
  | -- | @since 1.0
    MiscTherefore
  | -- | @since 1.0
    MiscBecause
  | -- | @since 1.0
    MiscLDots
  | -- | @since 1.0
    MiscCDots
  | -- | @since 1.0
    MiscVDots
  | -- | @since 1.0
    MiscDDots
  | -- | @since 1.0
    MiscSpace
  | -- | @since 1.0
    MiscQuad
  | -- | @since 1.0
    MiscAngle
  | -- | @since 1.0
    MiscFrown
  | -- | @since 1.0
    MiscTriangle
  | -- | @since 1.0
    MiscDiamond
  | -- | @since 1.0
    MiscSquare
  | -- | @since 1.0
    MiscC
  | -- | @since 1.0
    MiscN
  | -- | @since 1.0
    MiscQ
  | -- | @since 1.0
    MiscR
  | -- | @since 1.0
    MiscZ
  | -- | @since 1.0
    MiscFullStop
  | -- | @since 1.0
    MiscComma
  | -- | @since 1.0
    MiscSemicolon
  | -- | @since 1.0
    MiscColon
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Relation symbols.
--
-- @since 1.0
data RelationSymbol
  = -- | @since 1.0
    REq
  | -- | @since 1.0
    RNe
  | -- | @since 1.0
    RLt
  | -- | @since 1.0
    RGt
  | -- | @since 1.0
    RLe
  | -- | @since 1.0
    RGe
  | -- | @since 1.0
    RMLt
  | -- | @since 1.0
    RMGt
  | -- | @since 1.0
    RPreq
  | -- | @since 1.0
    RPreceq
  | -- | @since 1.0
    RSucc
  | -- | @since 1.0
    RSucceq
  | -- | @since 1.0
    RIn
  | -- | @since 1.0
    RNotIn
  | -- | @since 1.0
    RSubset
  | -- | @since 1.0
    RSupset
  | -- | @since 1.0
    RSubseteq
  | -- | @since 1.0
    RSupseteq
  | -- | @since 1.0
    REquiv
  | -- | @since 1.0
    RCong
  | -- | @since 1.0
    RApprox
  | -- | @since 1.0
    RProp
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Mathematical operation symbols.
--
-- @since 1.0
data MathOperation
  = -- | @since 1.0
    OpPlus
  | -- | @since 1.0
    OpMinus
  | -- | @since 1.0
    OpCDot
  | -- | @since 1.0
    OpAst
  | -- | @since 1.0
    OpStar
  | -- | @since 1.0
    OpForwardSlash
  | -- | @since 1.0
    OpBackslash
  | -- | @since 1.0
    OpTimes
  | -- | @since 1.0
    OpDiv
  | -- | @since 1.0
    OpLTimes
  | -- | @since 1.0
    OpRTimes
  | -- | @since 1.0
    OpBowtie
  | -- | @since 1.0
    OpCirc
  | -- | @since 1.0
    OpOPlus
  | -- | @since 1.0
    OpOTimes
  | -- | @since 1.0
    OpODot
  | -- | @since 1.0
    OpSum
  | -- | @since 1.0
    OpProduct
  | -- | @since 1.0
    OpWedge
  | -- | @since 1.0
    OpBigWedge
  | -- | @since 1.0
    OpVee
  | -- | @since 1.0
    OpBigVee
  | -- | @since 1.0
    OpCap
  | -- | @since 1.0
    OpBigCap
  | -- | @since 1.0
    OpCup
  | -- | @since 1.0
    OpBigCup
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Logic symbols.
--
-- @since 1.0
data LogicSymbol
  = -- | @since 1.0
    LAnd
  | -- | @since 1.0
    LOr
  | -- | @since 1.0
    LNot
  | -- | @since 1.0
    LImplies
  | -- | @since 1.0
    LIf
  | -- | @since 1.0
    LIff
  | -- | @since 1.0
    LForall
  | -- | @since 1.0
    LExists
  | -- | @since 1.0
    LBot
  | -- | @since 1.0
    LTop
  | -- | @since 1.0
    LTurnstile
  | -- | @since 1.0
    LModels
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Arrows
--
-- @since 1.0
data MathArrow
  = -- | @since 1.0
    UpArrow
  | -- | @since 1.0
    DownArrow
  | -- | @since 1.0
    RightArrow
  | -- | @since 1.0
    RightArrowThick
  | -- | @since 1.0
    LeftArrow
  | -- | @since 1.0
    LeftArrowThick
  | -- | @since 1.0
    LeftRightArrow
  | -- | @since 1.0
    LeftRightArrowThick
  | -- | @since 1.0
    RightArrowTail
  | -- | @since 1.0
    TwoHeadRightArrow
  | -- | @since 1.0
    TwoHeadRightArrowTail
  | -- | @since 1.0
    MapsTo
  | -- | @since 1.0
    ArrowTo
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | Greek letters.
--
-- @since 1.0
data GreekLetter
  = -- | @since 1.0
    Alpha
  | -- | @since 1.0
    Beta
  | -- | @since 1.0
    SmallGamma
  | -- | @since 1.0
    BigGamma
  | -- | @since 1.0
    SmallDelta
  | -- | @since 1.0
    BigDelta
  | -- | @since 1.0
    Epsilon
  | -- | @since 1.0
    VarEpsilon
  | -- | @since 1.0
    Zeta
  | -- | @since 1.0
    Eta
  | -- | @since 1.0
    SmallTheta
  | -- | @since 1.0
    BigTheta
  | -- | @since 1.0
    VarTheta
  | -- | @since 1.0
    Iota
  | -- | @since 1.0
    Kappa
  | -- | @since 1.0
    SmallLambda
  | -- | @since 1.0
    BigLambda
  | -- | @since 1.0
    Mu
  | -- | @since 1.0
    Nu
  | -- | @since 1.0
    SmallXi
  | -- | @since 1.0
    BigXi
  | -- | @since 1.0
    SmallPi
  | -- | @since 1.0
    BigPi
  | -- | @since 1.0
    Rho
  | -- | @since 1.0
    SmallSigma
  | -- | @since 1.0
    BigSigma
  | -- | @since 1.0
    Tau
  | -- | @since 1.0
    Upsilon
  | -- | @since 1.0
    SmallPhi
  | -- | @since 1.0
    BigPhi
  | -- | @since 1.0
    VarPhi
  | -- | @since 1.0
    Chi
  | -- | @since 1.0
    SmallPsi
  | -- | @since 1.0
    BigPsi
  | -- | @since 1.0
    SmallOmega
  | -- | @since 1.0
    BigOmega
  deriving stock
    ( -- | @since 1.0
      Eq,
      -- | @since 1.0
      Show
    )

-- | @since 1.0
parseMathSymbol :: AsciiMathParser MathSymbol
parseMathSymbol =
  (getAlt . Vector.foldMap' go $ parseLut)
    <|> try parseNumber
    <|> CharS <$> satisfy isSuitableSymbol
  where
    go :: (Text, MathSymbol) -> Alt AsciiMathParser MathSymbol
    go (t, res) = Alt (chunk t $> res)
    parseNumber :: AsciiMathParser MathSymbol
    parseNumber = do
      ds <- NEVector.fromNonEmpty <$> some parseDigit
      mds <- optional (chunk "." *> (NEVector.fromNonEmpty <$> some parseDigit))
      pure . NumberS . AsciiNumber ds $ mds
    parseDigit :: AsciiMathParser AsciiDigit
    parseDigit =
      (chunk "0" $> DZero)
        <|> (chunk "1" $> DOne)
        <|> (chunk "2" $> DTwo)
        <|> (chunk "3" $> DThree)
        <|> (chunk "4" $> DFour)
        <|> (chunk "5" $> DFive)
        <|> (chunk "6" $> DSix)
        <|> (chunk "7" $> DSeven)
        <|> (chunk "8" $> DEight)
        <|> (chunk "9" $> DNine)
    isSuitableSymbol :: Char -> Bool
    isSuitableSymbol c
      | not . isPrint $ c = False
      | isSpace c = False
      | c `elem` forbiddenSymbols = False
      | otherwise = True
    forbiddenSymbols :: [Char]
    forbiddenSymbols = "*+-@=><,.0123456789:;"

-- Helpers

-- This allows us to check by table lookup.
-- We sort by _longest_ match, as this ensures we don't get conflicts if a
-- shorter match gets tried before a longer one.
--
-- No really, there isn't a better way than this.
parseLut :: Vector (Text, MathSymbol)
parseLut =
  Vector.fromList . sortOn (Down . Text.length . fst) $
    [ -- Greek letters
      ("alpha", GreekS Alpha),
      ("beta", GreekS Beta),
      ("gamma", GreekS SmallGamma),
      ("Gamma", GreekS BigGamma),
      ("delta", GreekS SmallDelta),
      ("Delta", GreekS BigDelta),
      ("epsilon", GreekS Epsilon),
      ("varepsilon", GreekS VarEpsilon),
      ("zeta", GreekS Zeta),
      ("eta", GreekS Eta),
      ("theta", GreekS SmallTheta),
      ("Theta", GreekS BigTheta),
      ("vartheta", GreekS VarTheta),
      ("iota", GreekS Iota),
      ("kappa", GreekS Kappa),
      ("lambda", GreekS SmallLambda),
      ("Lambda", GreekS BigLambda),
      ("mu", GreekS Mu),
      ("nu", GreekS Nu),
      ("xi", GreekS SmallXi),
      ("Xi", GreekS BigXi),
      ("pi", GreekS SmallPi),
      ("Pi", GreekS BigPi),
      ("rho", GreekS Rho),
      ("sigma", GreekS SmallSigma),
      ("Sigma", GreekS BigSigma),
      ("tau", GreekS Tau),
      ("upsilon", GreekS Upsilon),
      ("phi", GreekS SmallPhi),
      ("Phi", GreekS BigPhi),
      ("varphi", GreekS VarPhi),
      ("chi", GreekS Chi),
      ("psi", GreekS SmallPsi),
      ("Psi", GreekS BigPsi),
      ("omega", GreekS SmallOmega),
      ("Omega", GreekS BigOmega),
      -- Arrows
      ("uarr", ArrowS UpArrow),
      ("uparrow", ArrowS UpArrow),
      ("darr", ArrowS DownArrow),
      ("downarrow", ArrowS DownArrow),
      ("rarr", ArrowS RightArrow),
      ("rightarrow", ArrowS RightArrow),
      ("->>", ArrowS TwoHeadRightArrow),
      ("twoheadrightarrow", ArrowS TwoHeadRightArrow),
      ("->", ArrowS ArrowTo),
      ("to", ArrowS ArrowTo),
      (">->>", ArrowS TwoHeadRightArrowTail),
      ("twoheadrightarrowtail", ArrowS TwoHeadRightArrowTail),
      (">->", ArrowS RightArrowTail),
      ("rightarrowtail", ArrowS RightArrowTail),
      ("|->", ArrowS MapsTo),
      ("mapsto", ArrowS MapsTo),
      ("larr", ArrowS LeftArrow),
      ("leftarrow", ArrowS LeftArrow),
      ("harr", ArrowS LeftRightArrow),
      ("leftrightarrow", ArrowS LeftRightArrow),
      ("lArr", ArrowS LeftArrowThick),
      ("Leftarrow", ArrowS LeftArrowThick),
      ("rArr", ArrowS RightArrowThick),
      ("Rightarrow", ArrowS RightArrowThick),
      ("hArr", ArrowS LeftRightArrowThick),
      ("Leftrightarrow", ArrowS LeftRightArrowThick),
      -- Logic symbols
      ("and", LogicS LAnd),
      ("or", LogicS LOr),
      ("not", LogicS LNot),
      ("neg", LogicS LNot),
      ("=>", LogicS LImplies),
      ("implies", LogicS LImplies),
      ("if", LogicS LIf),
      ("<=>", LogicS LIff),
      ("iff", LogicS LIff),
      ("AA", LogicS LForall),
      ("forall", LogicS LForall),
      ("EE", LogicS LExists),
      ("exists", LogicS LExists),
      ("_|_", LogicS LBot),
      ("bot", LogicS LBot),
      ("TT", LogicS LTop),
      ("top", LogicS LTop),
      ("|--", LogicS LTurnstile),
      ("vdash", LogicS LTurnstile),
      ("|==", LogicS LModels),
      ("models", LogicS LModels),
      -- Relational symbols
      ("=", RelationS REq),
      ("!=", RelationS RNe),
      ("ne", RelationS RNe),
      ("<=", RelationS RLe),
      ("le", RelationS RLe),
      ("<", RelationS RLt),
      ("lt", RelationS RLt),
      (">=", RelationS RGe),
      ("ge", RelationS RGe),
      (">-=", RelationS RSucceq),
      ("succeq", RelationS RSucceq),
      (">-", RelationS RSucc),
      ("succ", RelationS RSucc),
      ("><|", OperationS OpRTimes),
      ("rtimes", OperationS OpRTimes),
      (">", RelationS RGt),
      ("gt", RelationS RGt),
      ("mlt", RelationS RMLt),
      ("ll", RelationS RMLt),
      ("mgt", RelationS RMGt),
      ("gg", RelationS RMGt),
      ("-<=", RelationS RPreceq),
      ("preceq", RelationS RPreceq),
      ("-<", RelationS RPreq),
      ("prec", RelationS RPreq),
      ("in", RelationS RIn),
      ("!in", RelationS RNotIn),
      ("notin", RelationS RNotIn),
      ("sube", RelationS RSubseteq),
      ("subseteq", RelationS RSubseteq),
      ("sub", RelationS RSubset),
      ("subset", RelationS RSubset),
      ("supe", RelationS RSupseteq),
      ("supseteq", RelationS RSupseteq),
      ("sup", RelationS RSupset),
      ("supset", RelationS RSupset),
      ("-=", RelationS REquiv),
      ("equiv", RelationS REquiv),
      ("~=", RelationS RCong),
      ("cong", RelationS RCong),
      ("~~", RelationS RApprox),
      ("approx", RelationS RApprox),
      ("prop", RelationS RProp),
      ("propto", RelationS RProp),
      -- Operation symbols
      ("+", OperationS OpPlus),
      ("-:", OperationS OpDiv),
      ("div", OperationS OpDiv),
      ("-", OperationS OpMinus),
      ("***", OperationS OpStar),
      ("star", OperationS OpStar),
      ("**", OperationS OpAst),
      ("ast", OperationS OpAst),
      ("*", OperationS OpCDot),
      ("cdot", OperationS OpCDot),
      ("//", OperationS OpForwardSlash),
      ("\\\\", OperationS OpBackslash),
      ("backslash", OperationS OpBackslash),
      ("setminus", OperationS OpBackslash),
      ("xx", OperationS OpTimes),
      ("times", OperationS OpTimes),
      ("|><|", OperationS OpBowtie),
      ("bowtie", OperationS OpBowtie),
      ("|><", OperationS OpLTimes),
      ("ltimes", OperationS OpLTimes),
      ("@", OperationS OpCirc),
      ("circ", OperationS OpCirc),
      ("o+", OperationS OpOPlus),
      ("oplus", OperationS OpOPlus),
      ("ox", OperationS OpOTimes),
      ("otimes", OperationS OpOTimes),
      ("o.", OperationS OpODot),
      ("odot", OperationS OpODot),
      ("sum", OperationS OpSum),
      ("product", OperationS OpProduct),
      ("^^^", OperationS OpBigWedge),
      ("bigwedge", OperationS OpBigWedge),
      ("^^", OperationS OpWedge),
      ("wedge", OperationS OpWedge),
      ("vvv", OperationS OpBigVee),
      ("bigvee", OperationS OpBigVee),
      ("vv", OperationS OpVee),
      ("vee", OperationS OpVee),
      ("nnn", OperationS OpBigCap),
      ("bigcap", OperationS OpBigCap),
      ("nn", OperationS OpCap),
      ("cap", OperationS OpCap),
      ("uuu", OperationS OpBigCup),
      ("bigcup", OperationS OpBigCup),
      ("uu", OperationS OpCup),
      ("cup", OperationS OpCup),
      -- Miscellaneous symbols
      ("int", MiscS MiscInt),
      ("+-", MiscS MiscPM),
      ("plusminus", MiscS MiscPM),
      ("oint", MiscS MiscOInt),
      ("del", MiscS MiscDel),
      ("partial", MiscS MiscDel),
      ("grad", MiscS MiscGrad),
      ("nabla", MiscS MiscGrad),
      ("O/", MiscS MiscEmptyset),
      ("emptyset", MiscS MiscEmptyset),
      ("oo", MiscS MiscInfinity),
      ("infty", MiscS MiscInfinity),
      ("aleph", MiscS MiscAleph),
      (":.", MiscS MiscTherefore),
      ("therefore", MiscS MiscTherefore),
      (":'", MiscS MiscBecause),
      ("because", MiscS MiscBecause),
      ("...", MiscS MiscLDots),
      ("ldots", MiscS MiscLDots),
      ("cdots", MiscS MiscCDots),
      ("vdots", MiscS MiscVDots),
      ("ddots", MiscS MiscDDots),
      ("\\ ", MiscS MiscSpace),
      ("quad", MiscS MiscQuad),
      ("/_\\", MiscS MiscTriangle),
      ("triangle", MiscS MiscTriangle),
      ("/_", MiscS MiscAngle),
      ("angle", MiscS MiscAngle),
      ("frown", MiscS MiscFrown),
      ("diamond", MiscS MiscDiamond),
      ("square", MiscS MiscSquare),
      ("CC", MiscS MiscC),
      ("NN", MiscS MiscN),
      ("QQ", MiscS MiscQ),
      ("RR", MiscS MiscR),
      ("ZZ", MiscS MiscZ),
      (".", MiscS MiscFullStop),
      (",", MiscS MiscComma),
      (";", MiscS MiscSemicolon),
      (":", MiscS MiscColon),
      -- Functions
      ("sin", FunctionS SinF),
      ("cos", FunctionS CosF),
      ("tan", FunctionS TanF),
      ("sec", FunctionS SecF),
      ("csc", FunctionS CscF),
      ("cot", FunctionS CotF),
      ("arcsin", FunctionS ArcsinF),
      ("arccos", FunctionS ArccosF),
      ("arctan", FunctionS ArctanF),
      ("sinh", FunctionS SinhF),
      ("cosh", FunctionS CoshF),
      ("tanh", FunctionS TanhF),
      ("sech", FunctionS SechF),
      ("csch", FunctionS CschF),
      ("coth", FunctionS CothF),
      ("exp", FunctionS ExpF),
      ("log", FunctionS LogF),
      ("ln", FunctionS LnF),
      ("det", FunctionS DetF),
      ("dim", FunctionS DimF),
      ("mod", FunctionS ModF),
      ("gcd", FunctionS GcdF),
      ("lcm", FunctionS LcmF),
      ("lub", FunctionS LubF),
      ("glb", FunctionS GlbF),
      ("min", FunctionS MinF),
      ("max", FunctionS MaxF)
    ]
