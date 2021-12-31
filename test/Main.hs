{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Ascii.Math.Parser (parseMathMaybe)
import Data.Ascii.Math.Symbol
  ( AsciiCase (CaseLower, CaseUpper),
    AsciiDigit,
    MathLetter (MathLetter),
    MathSymbol,
    parseMathSymbol,
    _Arrow,
    _Function,
    _Greek,
    _Letter,
    _Logic,
    _Misc,
    _Number,
    _Operation,
    _Relation,
  )
import qualified Data.Ascii.Math.Symbol as Symbol
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEVector
import Hedgehog
  ( Gen,
    cover,
    failure,
    forAllWith,
    property,
    withTests,
    (===),
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Optics.AffineFold (preview)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Text.Show.Pretty (ppShow)

main :: IO ()
main =
  defaultMain . testGroup "Tests" $
    [ testProperty "'Leaf' symbols parse" . withTests 4000 . property $ do
        (input, expected) <- forAllWith ppShow pickFromLut
        cover 10 "Greek letter" (isJust . preview _Greek $ expected)
        cover 10 "Arrow" (isJust . preview _Arrow $ expected)
        cover 10 "Logic symbol" (isJust . preview _Logic $ expected)
        cover 10 "Relational symbol" (isJust . preview _Relation $ expected)
        cover 10 "Operation" (isJust . preview _Operation $ expected)
        cover 10 "Miscellaneous" (isJust . preview _Misc $ expected)
        cover 10 "Letter" (isJust . preview _Letter $ expected)
        cover 10 "Function symbol" (isJust . preview _Function $ expected)
        cover 10 "Number" (isJust . preview _Number $ expected)
        case parseMathMaybe parseMathSymbol input of
          Nothing -> failure
          Just y -> y === expected
    ]
  where
    pickFromLut :: Gen (Text, MathSymbol)
    pickFromLut = Gen.choice . Vector.toList $ testLut

-- Helpers

testLut :: Vector (Gen (Text, MathSymbol))
testLut =
  Vector.fromList
    [ go greekLut,
      go arrowLut,
      go logicLut,
      go relationLut,
      go operationLut,
      go miscLut,
      go letterLut,
      go functionLut,
      genNumber
    ]
  where
    go :: Vector (Text, MathSymbol) -> Gen (Text, MathSymbol)
    go = Gen.element . Vector.toList

genNumber :: Gen (Text, MathSymbol)
genNumber = do
  (t, ds) <- genDigitSequence
  mDec <- Gen.maybe genDigitSequence
  case mDec of
    Nothing -> pure (t, Symbol.NumberS . Symbol.AsciiNumber ds $ Nothing)
    Just (t', ds') -> do
      let t'' = t <> "." <> t'
      pure (t'', Symbol.NumberS . Symbol.AsciiNumber ds . Just $ ds')

genDigitSequence :: Gen (Text, NonEmptyVector AsciiDigit)
genDigitSequence = do
  nev <- NEVector.fromNonEmpty <$> Gen.nonEmpty (Range.linear 0 100) go
  let t = foldMap toText nev
  pure (t, nev)
  where
    go :: Gen AsciiDigit
    go =
      Gen.element
        [ Symbol.DZero,
          Symbol.DOne,
          Symbol.DTwo,
          Symbol.DThree,
          Symbol.DFour,
          Symbol.DFive,
          Symbol.DSix,
          Symbol.DSeven,
          Symbol.DEight,
          Symbol.DNine
        ]
    toText :: AsciiDigit -> Text
    toText = \case
      Symbol.DZero -> "0"
      Symbol.DOne -> "1"
      Symbol.DTwo -> "2"
      Symbol.DThree -> "3"
      Symbol.DFour -> "4"
      Symbol.DFive -> "5"
      Symbol.DSix -> "6"
      Symbol.DSeven -> "7"
      Symbol.DEight -> "8"
      Symbol.DNine -> "9"

greekLut :: Vector (Text, MathSymbol)
greekLut =
  Vector.fromList . fmap (fmap Symbol.GreekS) $
    [ ("alpha", Symbol.Alpha),
      ("beta", Symbol.Beta),
      ("gamma", Symbol.SmallGamma),
      ("Gamma", Symbol.BigGamma),
      ("delta", Symbol.SmallDelta),
      ("Delta", Symbol.BigDelta),
      ("epsilon", Symbol.Epsilon),
      ("varepsilon", Symbol.VarEpsilon),
      ("zeta", Symbol.Zeta),
      ("eta", Symbol.Eta),
      ("theta", Symbol.SmallTheta),
      ("Theta", Symbol.BigTheta),
      ("vartheta", Symbol.VarTheta),
      ("iota", Symbol.Iota),
      ("kappa", Symbol.Kappa),
      ("lambda", Symbol.SmallLambda),
      ("Lambda", Symbol.BigLambda),
      ("mu", Symbol.Mu),
      ("nu", Symbol.Nu),
      ("xi", Symbol.SmallXi),
      ("Xi", Symbol.BigXi),
      ("pi", Symbol.SmallPi),
      ("Pi", Symbol.BigPi),
      ("rho", Symbol.Rho),
      ("sigma", Symbol.SmallSigma),
      ("Sigma", Symbol.BigSigma),
      ("tau", Symbol.Tau),
      ("upsilon", Symbol.Upsilon),
      ("phi", Symbol.SmallPhi),
      ("Phi", Symbol.BigPhi),
      ("varphi", Symbol.VarPhi),
      ("chi", Symbol.Chi),
      ("psi", Symbol.SmallPsi),
      ("Psi", Symbol.BigPsi),
      ("omega", Symbol.SmallOmega),
      ("Omega", Symbol.BigOmega)
    ]

arrowLut :: Vector (Text, MathSymbol)
arrowLut =
  Vector.fromList . fmap (fmap Symbol.ArrowS) $
    [ ("uarr", Symbol.UpArrow),
      ("uparrow", Symbol.UpArrow),
      ("darr", Symbol.DownArrow),
      ("downarrow", Symbol.DownArrow),
      ("rarr", Symbol.RightArrow),
      ("rightarrow", Symbol.RightArrow),
      ("->>", Symbol.TwoHeadRightArrow),
      ("twoheadrightarrow", Symbol.TwoHeadRightArrow),
      ("->", Symbol.ArrowTo),
      ("to", Symbol.ArrowTo),
      (">->>", Symbol.TwoHeadRightArrowTail),
      ("twoheadrightarrowtail", Symbol.TwoHeadRightArrowTail),
      (">->", Symbol.RightArrowTail),
      ("rightarrowtail", Symbol.RightArrowTail),
      ("|->", Symbol.MapsTo),
      ("mapsto", Symbol.MapsTo),
      ("larr", Symbol.LeftArrow),
      ("leftarrow", Symbol.LeftArrow),
      ("harr", Symbol.LeftRightArrow),
      ("leftrightarrow", Symbol.LeftRightArrow),
      ("lArr", Symbol.LeftArrowThick),
      ("Leftarrow", Symbol.LeftArrowThick),
      ("rArr", Symbol.RightArrowThick),
      ("Rightarrow", Symbol.RightArrowThick),
      ("hArr", Symbol.LeftRightArrowThick),
      ("Leftrightarrow", Symbol.LeftRightArrowThick)
    ]

logicLut :: Vector (Text, MathSymbol)
logicLut =
  Vector.fromList . fmap (fmap Symbol.LogicS) $
    [ ("and", Symbol.LAnd),
      ("or", Symbol.LOr),
      ("not", Symbol.LNot),
      ("neg", Symbol.LNot),
      ("=>", Symbol.LImplies),
      ("implies", Symbol.LImplies),
      ("if", Symbol.LIf),
      ("<=>", Symbol.LIff),
      ("iff", Symbol.LIff),
      ("AA", Symbol.LForall),
      ("forall", Symbol.LForall),
      ("EE", Symbol.LExists),
      ("exists", Symbol.LExists),
      ("_|_", Symbol.LBot),
      ("bot", Symbol.LBot),
      ("TT", Symbol.LTop),
      ("top", Symbol.LTop),
      ("|--", Symbol.LTurnstile),
      ("vdash", Symbol.LTurnstile),
      ("|==", Symbol.LModels),
      ("models", Symbol.LModels)
    ]

relationLut :: Vector (Text, MathSymbol)
relationLut =
  Vector.fromList . fmap (fmap Symbol.RelationS) $
    [ ("in", Symbol.RIn),
      ("=", Symbol.REq),
      ("!=", Symbol.RNe),
      ("ne", Symbol.RNe),
      ("<=", Symbol.RLe),
      ("le", Symbol.RLe),
      ("<", Symbol.RLt),
      ("lt", Symbol.RLt),
      (">=", Symbol.RGe),
      ("ge", Symbol.RGe),
      (">-=", Symbol.RSucceq),
      ("succeq", Symbol.RSucceq),
      (">-", Symbol.RSucc),
      ("succ", Symbol.RSucc),
      (">", Symbol.RGt),
      ("gt", Symbol.RGt),
      ("mlt", Symbol.RMLt),
      ("ll", Symbol.RMLt),
      ("mgt", Symbol.RMGt),
      ("gg", Symbol.RMGt),
      ("-<=", Symbol.RPreceq),
      ("preceq", Symbol.RPreceq),
      ("-<", Symbol.RPreq),
      ("prec", Symbol.RPreq),
      ("!in", Symbol.RNotIn),
      ("notin", Symbol.RNotIn),
      ("sube", Symbol.RSubseteq),
      ("subseteq", Symbol.RSubseteq),
      ("sub", Symbol.RSubset),
      ("subset", Symbol.RSubset),
      ("supe", Symbol.RSupseteq),
      ("supseteq", Symbol.RSupseteq),
      ("sup", Symbol.RSupset),
      ("supset", Symbol.RSupset),
      ("-=", Symbol.REquiv),
      ("equiv", Symbol.REquiv),
      ("~=", Symbol.RCong),
      ("cong", Symbol.RCong),
      ("~~", Symbol.RApprox),
      ("approx", Symbol.RApprox),
      ("prop", Symbol.RProp),
      ("propto", Symbol.RProp)
    ]

operationLut :: Vector (Text, MathSymbol)
operationLut =
  Vector.fromList . fmap (fmap Symbol.OperationS) $
    [ ("+", Symbol.OpPlus),
      ("-:", Symbol.OpDiv),
      ("><|", Symbol.OpRTimes),
      ("rtimes", Symbol.OpRTimes),
      ("div", Symbol.OpDiv),
      ("-", Symbol.OpMinus),
      ("***", Symbol.OpStar),
      ("star", Symbol.OpStar),
      ("**", Symbol.OpAst),
      ("ast", Symbol.OpAst),
      ("*", Symbol.OpCDot),
      ("cdot", Symbol.OpCDot),
      ("//", Symbol.OpForwardSlash),
      ("\\\\", Symbol.OpBackslash),
      ("backslash", Symbol.OpBackslash),
      ("setminus", Symbol.OpBackslash),
      ("xx", Symbol.OpTimes),
      ("times", Symbol.OpTimes),
      ("|><|", Symbol.OpBowtie),
      ("bowtie", Symbol.OpBowtie),
      ("|><", Symbol.OpLTimes),
      ("ltimes", Symbol.OpLTimes),
      ("@", Symbol.OpCirc),
      ("circ", Symbol.OpCirc),
      ("o+", Symbol.OpOPlus),
      ("oplus", Symbol.OpOPlus),
      ("ox", Symbol.OpOTimes),
      ("otimes", Symbol.OpOTimes),
      ("o.", Symbol.OpODot),
      ("odot", Symbol.OpODot),
      ("sum", Symbol.OpSum),
      ("product", Symbol.OpProduct),
      ("^^^", Symbol.OpBigWedge),
      ("bigwedge", Symbol.OpBigWedge),
      ("^^", Symbol.OpWedge),
      ("wedge", Symbol.OpWedge),
      ("vvv", Symbol.OpBigVee),
      ("bigvee", Symbol.OpBigVee),
      ("vv", Symbol.OpVee),
      ("vee", Symbol.OpVee),
      ("nnn", Symbol.OpBigCap),
      ("bigcap", Symbol.OpBigCap),
      ("nn", Symbol.OpCap),
      ("cap", Symbol.OpCap),
      ("uuu", Symbol.OpBigCup),
      ("bigcup", Symbol.OpBigCup),
      ("uu", Symbol.OpCup),
      ("cup", Symbol.OpCup)
    ]

miscLut :: Vector (Text, MathSymbol)
miscLut =
  Vector.fromList . fmap (fmap Symbol.MiscS) $
    [ ("int", Symbol.MiscInt),
      ("+-", Symbol.MiscPM),
      ("plusminus", Symbol.MiscPM),
      ("oint", Symbol.MiscOInt),
      ("del", Symbol.MiscDel),
      ("partial", Symbol.MiscDel),
      ("grad", Symbol.MiscGrad),
      ("nabla", Symbol.MiscGrad),
      ("O/", Symbol.MiscEmptyset),
      ("emptyset", Symbol.MiscEmptyset),
      ("oo", Symbol.MiscInfinity),
      ("infty", Symbol.MiscInfinity),
      ("aleph", Symbol.MiscAleph),
      (":.", Symbol.MiscTherefore),
      ("therefore", Symbol.MiscTherefore),
      (":'", Symbol.MiscBecause),
      ("because", Symbol.MiscBecause),
      ("...", Symbol.MiscLDots),
      ("ldots", Symbol.MiscLDots),
      ("cdots", Symbol.MiscCDots),
      ("vdots", Symbol.MiscVDots),
      ("ddots", Symbol.MiscDDots),
      ("\\ ", Symbol.MiscSpace),
      ("quad", Symbol.MiscQuad),
      ("/_\\", Symbol.MiscTriangle),
      ("triangle", Symbol.MiscTriangle),
      ("/_", Symbol.MiscAngle),
      ("angle", Symbol.MiscAngle),
      ("frown", Symbol.MiscFrown),
      ("diamond", Symbol.MiscDiamond),
      ("square", Symbol.MiscSquare),
      ("CC", Symbol.MiscC),
      ("NN", Symbol.MiscN),
      ("QQ", Symbol.MiscQ),
      ("RR", Symbol.MiscR),
      ("ZZ", Symbol.MiscZ),
      (".", Symbol.MiscFullStop),
      (",", Symbol.MiscComma),
      (";", Symbol.MiscSemicolon),
      (":", Symbol.MiscColon)
    ]

letterLut :: Vector (Text, MathSymbol)
letterLut =
  Vector.fromList . fmap (fmap Symbol.LetterSy) $
    [ ("a", MathLetter Symbol.LetterA CaseLower),
      ("A", MathLetter Symbol.LetterA CaseUpper),
      ("b", MathLetter Symbol.LetterB CaseLower),
      ("B", MathLetter Symbol.LetterB CaseUpper),
      ("c", MathLetter Symbol.LetterC CaseLower),
      ("C", MathLetter Symbol.LetterC CaseUpper),
      ("d", MathLetter Symbol.LetterD CaseLower),
      ("D", MathLetter Symbol.LetterD CaseUpper),
      ("e", MathLetter Symbol.LetterE CaseLower),
      ("E", MathLetter Symbol.LetterE CaseUpper),
      ("f", MathLetter Symbol.LetterF CaseLower),
      ("F", MathLetter Symbol.LetterF CaseUpper),
      ("g", MathLetter Symbol.LetterG CaseLower),
      ("G", MathLetter Symbol.LetterG CaseUpper),
      ("h", MathLetter Symbol.LetterH CaseLower),
      ("H", MathLetter Symbol.LetterH CaseUpper),
      ("i", MathLetter Symbol.LetterI CaseLower),
      ("I", MathLetter Symbol.LetterI CaseUpper),
      ("j", MathLetter Symbol.LetterJ CaseLower),
      ("J", MathLetter Symbol.LetterJ CaseUpper),
      ("k", MathLetter Symbol.LetterK CaseLower),
      ("K", MathLetter Symbol.LetterK CaseUpper),
      ("l", MathLetter Symbol.LetterL CaseLower),
      ("L", MathLetter Symbol.LetterL CaseUpper),
      ("m", MathLetter Symbol.LetterM CaseLower),
      ("M", MathLetter Symbol.LetterM CaseUpper),
      ("n", MathLetter Symbol.LetterN CaseLower),
      ("N", MathLetter Symbol.LetterN CaseUpper),
      ("o", MathLetter Symbol.LetterO CaseLower),
      ("O", MathLetter Symbol.LetterO CaseUpper),
      ("p", MathLetter Symbol.LetterP CaseLower),
      ("P", MathLetter Symbol.LetterP CaseUpper),
      ("q", MathLetter Symbol.LetterQ CaseLower),
      ("Q", MathLetter Symbol.LetterQ CaseUpper),
      ("r", MathLetter Symbol.LetterR CaseLower),
      ("R", MathLetter Symbol.LetterR CaseUpper),
      ("s", MathLetter Symbol.LetterS CaseLower),
      ("S", MathLetter Symbol.LetterS CaseUpper),
      ("t", MathLetter Symbol.LetterT CaseLower),
      ("T", MathLetter Symbol.LetterT CaseUpper),
      ("u", MathLetter Symbol.LetterU CaseLower),
      ("U", MathLetter Symbol.LetterU CaseUpper),
      ("v", MathLetter Symbol.LetterV CaseLower),
      ("V", MathLetter Symbol.LetterV CaseUpper),
      ("w", MathLetter Symbol.LetterW CaseLower),
      ("W", MathLetter Symbol.LetterW CaseUpper),
      ("x", MathLetter Symbol.LetterX CaseLower),
      ("X", MathLetter Symbol.LetterX CaseUpper),
      ("y", MathLetter Symbol.LetterY CaseLower),
      ("Y", MathLetter Symbol.LetterY CaseUpper),
      ("z", MathLetter Symbol.LetterZ CaseLower),
      ("Z", MathLetter Symbol.LetterZ CaseUpper)
    ]

functionLut :: Vector (Text, MathSymbol)
functionLut =
  Vector.fromList
    [ ("sin", Symbol.FunctionS Symbol.SinF),
      ("cos", Symbol.FunctionS Symbol.CosF),
      ("tan", Symbol.FunctionS Symbol.TanF),
      ("sec", Symbol.FunctionS Symbol.SecF),
      ("csc", Symbol.FunctionS Symbol.CscF),
      ("cot", Symbol.FunctionS Symbol.CotF),
      ("arcsin", Symbol.FunctionS Symbol.ArcsinF),
      ("arccos", Symbol.FunctionS Symbol.ArccosF),
      ("arctan", Symbol.FunctionS Symbol.ArctanF),
      ("sinh", Symbol.FunctionS Symbol.SinhF),
      ("cosh", Symbol.FunctionS Symbol.CoshF),
      ("tanh", Symbol.FunctionS Symbol.TanhF),
      ("sech", Symbol.FunctionS Symbol.SechF),
      ("csch", Symbol.FunctionS Symbol.CschF),
      ("coth", Symbol.FunctionS Symbol.CothF),
      ("exp", Symbol.FunctionS Symbol.ExpF),
      ("log", Symbol.FunctionS Symbol.LogF),
      ("ln", Symbol.FunctionS Symbol.LnF),
      ("det", Symbol.FunctionS Symbol.DetF),
      ("dim", Symbol.FunctionS Symbol.DimF),
      ("mod", Symbol.FunctionS Symbol.ModF),
      ("gcd", Symbol.FunctionS Symbol.GcdF),
      ("lcm", Symbol.FunctionS Symbol.LcmF),
      ("lub", Symbol.FunctionS Symbol.LubF),
      ("glb", Symbol.FunctionS Symbol.GlbF),
      ("min", Symbol.FunctionS Symbol.MinF),
      ("max", Symbol.FunctionS Symbol.MaxF)
    ]
