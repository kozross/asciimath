{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Ascii.Math
  ( AsciiCase (CaseLower, CaseUpper),
    AsciiDigit,
    MathLetter (MathLetter),
    MathSymbol,
    parseMathMaybe,
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
import qualified Data.Ascii.Math as Math
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
    Nothing -> pure (t, Math.NumberS . Math.AsciiNumber ds $ Nothing)
    Just (t', ds') -> do
      let t'' = t <> "." <> t'
      pure (t'', Math.NumberS . Math.AsciiNumber ds . Just $ ds')

genDigitSequence :: Gen (Text, NonEmptyVector AsciiDigit)
genDigitSequence = do
  nev <- NEVector.fromNonEmpty <$> Gen.nonEmpty (Range.linear 0 100) go
  let t = foldMap toText nev
  pure (t, nev)
  where
    go :: Gen AsciiDigit
    go =
      Gen.element
        [ Math.DZero,
          Math.DOne,
          Math.DTwo,
          Math.DThree,
          Math.DFour,
          Math.DFive,
          Math.DSix,
          Math.DSeven,
          Math.DEight,
          Math.DNine
        ]
    toText :: AsciiDigit -> Text
    toText = \case
      Math.DZero -> "0"
      Math.DOne -> "1"
      Math.DTwo -> "2"
      Math.DThree -> "3"
      Math.DFour -> "4"
      Math.DFive -> "5"
      Math.DSix -> "6"
      Math.DSeven -> "7"
      Math.DEight -> "8"
      Math.DNine -> "9"

greekLut :: Vector (Text, MathSymbol)
greekLut =
  Vector.fromList . fmap (fmap Math.GreekS) $
    [ ("alpha", Math.Alpha),
      ("beta", Math.Beta),
      ("gamma", Math.SmallGamma),
      ("Gamma", Math.BigGamma),
      ("delta", Math.SmallDelta),
      ("Delta", Math.BigDelta),
      ("epsilon", Math.Epsilon),
      ("varepsilon", Math.VarEpsilon),
      ("zeta", Math.Zeta),
      ("eta", Math.Eta),
      ("theta", Math.SmallTheta),
      ("Theta", Math.BigTheta),
      ("vartheta", Math.VarTheta),
      ("iota", Math.Iota),
      ("kappa", Math.Kappa),
      ("lambda", Math.SmallLambda),
      ("Lambda", Math.BigLambda),
      ("mu", Math.Mu),
      ("nu", Math.Nu),
      ("xi", Math.SmallXi),
      ("Xi", Math.BigXi),
      ("pi", Math.SmallPi),
      ("Pi", Math.BigPi),
      ("rho", Math.Rho),
      ("sigma", Math.SmallSigma),
      ("Sigma", Math.BigSigma),
      ("tau", Math.Tau),
      ("upsilon", Math.Upsilon),
      ("phi", Math.SmallPhi),
      ("Phi", Math.BigPhi),
      ("varphi", Math.VarPhi),
      ("chi", Math.Chi),
      ("psi", Math.SmallPsi),
      ("Psi", Math.BigPsi),
      ("omega", Math.SmallOmega),
      ("Omega", Math.BigOmega)
    ]

arrowLut :: Vector (Text, MathSymbol)
arrowLut =
  Vector.fromList . fmap (fmap Math.ArrowS) $
    [ ("uarr", Math.UpArrow),
      ("uparrow", Math.UpArrow),
      ("darr", Math.DownArrow),
      ("downarrow", Math.DownArrow),
      ("rarr", Math.RightArrow),
      ("rightarrow", Math.RightArrow),
      ("->>", Math.TwoHeadRightArrow),
      ("twoheadrightarrow", Math.TwoHeadRightArrow),
      ("->", Math.ArrowTo),
      ("to", Math.ArrowTo),
      (">->>", Math.TwoHeadRightArrowTail),
      ("twoheadrightarrowtail", Math.TwoHeadRightArrowTail),
      (">->", Math.RightArrowTail),
      ("rightarrowtail", Math.RightArrowTail),
      ("|->", Math.MapsTo),
      ("mapsto", Math.MapsTo),
      ("larr", Math.LeftArrow),
      ("leftarrow", Math.LeftArrow),
      ("harr", Math.LeftRightArrow),
      ("leftrightarrow", Math.LeftRightArrow),
      ("lArr", Math.LeftArrowThick),
      ("Leftarrow", Math.LeftArrowThick),
      ("rArr", Math.RightArrowThick),
      ("Rightarrow", Math.RightArrowThick),
      ("hArr", Math.LeftRightArrowThick),
      ("Leftrightarrow", Math.LeftRightArrowThick)
    ]

logicLut :: Vector (Text, MathSymbol)
logicLut =
  Vector.fromList . fmap (fmap Math.LogicS) $
    [ ("and", Math.LAnd),
      ("or", Math.LOr),
      ("not", Math.LNot),
      ("neg", Math.LNot),
      ("=>", Math.LImplies),
      ("implies", Math.LImplies),
      ("if", Math.LIf),
      ("<=>", Math.LIff),
      ("iff", Math.LIff),
      ("AA", Math.LForall),
      ("forall", Math.LForall),
      ("EE", Math.LExists),
      ("exists", Math.LExists),
      ("_|_", Math.LBot),
      ("bot", Math.LBot),
      ("TT", Math.LTop),
      ("top", Math.LTop),
      ("|--", Math.LTurnstile),
      ("vdash", Math.LTurnstile),
      ("|==", Math.LModels),
      ("models", Math.LModels)
    ]

relationLut :: Vector (Text, MathSymbol)
relationLut =
  Vector.fromList . fmap (fmap Math.RelationS) $
    [ ("in", Math.RIn),
      ("=", Math.REq),
      ("!=", Math.RNe),
      ("ne", Math.RNe),
      ("<=", Math.RLe),
      ("le", Math.RLe),
      ("<", Math.RLt),
      ("lt", Math.RLt),
      (">=", Math.RGe),
      ("ge", Math.RGe),
      (">-=", Math.RSucceq),
      ("succeq", Math.RSucceq),
      (">-", Math.RSucc),
      ("succ", Math.RSucc),
      (">", Math.RGt),
      ("gt", Math.RGt),
      ("mlt", Math.RMLt),
      ("ll", Math.RMLt),
      ("mgt", Math.RMGt),
      ("gg", Math.RMGt),
      ("-<=", Math.RPreceq),
      ("preceq", Math.RPreceq),
      ("-<", Math.RPreq),
      ("prec", Math.RPreq),
      ("!in", Math.RNotIn),
      ("notin", Math.RNotIn),
      ("sube", Math.RSubseteq),
      ("subseteq", Math.RSubseteq),
      ("sub", Math.RSubset),
      ("subset", Math.RSubset),
      ("supe", Math.RSupseteq),
      ("supseteq", Math.RSupseteq),
      ("sup", Math.RSupset),
      ("supset", Math.RSupset),
      ("-=", Math.REquiv),
      ("equiv", Math.REquiv),
      ("~=", Math.RCong),
      ("cong", Math.RCong),
      ("~~", Math.RApprox),
      ("approx", Math.RApprox),
      ("prop", Math.RProp),
      ("propto", Math.RProp)
    ]

operationLut :: Vector (Text, MathSymbol)
operationLut =
  Vector.fromList . fmap (fmap Math.OperationS) $
    [ ("+", Math.OpPlus),
      ("-:", Math.OpDiv),
      ("><|", Math.OpRTimes),
      ("rtimes", Math.OpRTimes),
      ("div", Math.OpDiv),
      ("-", Math.OpMinus),
      ("***", Math.OpStar),
      ("star", Math.OpStar),
      ("**", Math.OpAst),
      ("ast", Math.OpAst),
      ("*", Math.OpCDot),
      ("cdot", Math.OpCDot),
      ("//", Math.OpForwardSlash),
      ("\\\\", Math.OpBackslash),
      ("backslash", Math.OpBackslash),
      ("setminus", Math.OpBackslash),
      ("xx", Math.OpTimes),
      ("times", Math.OpTimes),
      ("|><|", Math.OpBowtie),
      ("bowtie", Math.OpBowtie),
      ("|><", Math.OpLTimes),
      ("ltimes", Math.OpLTimes),
      ("@", Math.OpCirc),
      ("circ", Math.OpCirc),
      ("o+", Math.OpOPlus),
      ("oplus", Math.OpOPlus),
      ("ox", Math.OpOTimes),
      ("otimes", Math.OpOTimes),
      ("o.", Math.OpODot),
      ("odot", Math.OpODot),
      ("sum", Math.OpSum),
      ("product", Math.OpProduct),
      ("^^^", Math.OpBigWedge),
      ("bigwedge", Math.OpBigWedge),
      ("^^", Math.OpWedge),
      ("wedge", Math.OpWedge),
      ("vvv", Math.OpBigVee),
      ("bigvee", Math.OpBigVee),
      ("vv", Math.OpVee),
      ("vee", Math.OpVee),
      ("nnn", Math.OpBigCap),
      ("bigcap", Math.OpBigCap),
      ("nn", Math.OpCap),
      ("cap", Math.OpCap),
      ("uuu", Math.OpBigCup),
      ("bigcup", Math.OpBigCup),
      ("uu", Math.OpCup),
      ("cup", Math.OpCup)
    ]

miscLut :: Vector (Text, MathSymbol)
miscLut =
  Vector.fromList . fmap (fmap Math.MiscS) $
    [ ("int", Math.MiscInt),
      ("+-", Math.MiscPM),
      ("plusminus", Math.MiscPM),
      ("oint", Math.MiscOInt),
      ("del", Math.MiscDel),
      ("partial", Math.MiscDel),
      ("grad", Math.MiscGrad),
      ("nabla", Math.MiscGrad),
      ("O/", Math.MiscEmptyset),
      ("emptyset", Math.MiscEmptyset),
      ("oo", Math.MiscInfinity),
      ("infty", Math.MiscInfinity),
      ("aleph", Math.MiscAleph),
      (":.", Math.MiscTherefore),
      ("therefore", Math.MiscTherefore),
      (":'", Math.MiscBecause),
      ("because", Math.MiscBecause),
      ("...", Math.MiscLDots),
      ("ldots", Math.MiscLDots),
      ("cdots", Math.MiscCDots),
      ("vdots", Math.MiscVDots),
      ("ddots", Math.MiscDDots),
      ("\\ ", Math.MiscSpace),
      ("quad", Math.MiscQuad),
      ("/_\\", Math.MiscTriangle),
      ("triangle", Math.MiscTriangle),
      ("/_", Math.MiscAngle),
      ("angle", Math.MiscAngle),
      ("frown", Math.MiscFrown),
      ("diamond", Math.MiscDiamond),
      ("square", Math.MiscSquare),
      ("CC", Math.MiscC),
      ("NN", Math.MiscN),
      ("QQ", Math.MiscQ),
      ("RR", Math.MiscR),
      ("ZZ", Math.MiscZ),
      (".", Math.MiscFullStop),
      (",", Math.MiscComma),
      (";", Math.MiscSemicolon),
      (":", Math.MiscColon)
    ]

letterLut :: Vector (Text, MathSymbol)
letterLut =
  Vector.fromList . fmap (fmap Math.LetterSy) $
    [ ("a", MathLetter Math.LetterA CaseLower),
      ("A", MathLetter Math.LetterA CaseUpper),
      ("b", MathLetter Math.LetterB CaseLower),
      ("B", MathLetter Math.LetterB CaseUpper),
      ("c", MathLetter Math.LetterC CaseLower),
      ("C", MathLetter Math.LetterC CaseUpper),
      ("d", MathLetter Math.LetterD CaseLower),
      ("D", MathLetter Math.LetterD CaseUpper),
      ("e", MathLetter Math.LetterE CaseLower),
      ("E", MathLetter Math.LetterE CaseUpper),
      ("f", MathLetter Math.LetterF CaseLower),
      ("F", MathLetter Math.LetterF CaseUpper),
      ("g", MathLetter Math.LetterG CaseLower),
      ("G", MathLetter Math.LetterG CaseUpper),
      ("h", MathLetter Math.LetterH CaseLower),
      ("H", MathLetter Math.LetterH CaseUpper),
      ("i", MathLetter Math.LetterI CaseLower),
      ("I", MathLetter Math.LetterI CaseUpper),
      ("j", MathLetter Math.LetterJ CaseLower),
      ("J", MathLetter Math.LetterJ CaseUpper),
      ("k", MathLetter Math.LetterK CaseLower),
      ("K", MathLetter Math.LetterK CaseUpper),
      ("l", MathLetter Math.LetterL CaseLower),
      ("L", MathLetter Math.LetterL CaseUpper),
      ("m", MathLetter Math.LetterM CaseLower),
      ("M", MathLetter Math.LetterM CaseUpper),
      ("n", MathLetter Math.LetterN CaseLower),
      ("N", MathLetter Math.LetterN CaseUpper),
      ("o", MathLetter Math.LetterO CaseLower),
      ("O", MathLetter Math.LetterO CaseUpper),
      ("p", MathLetter Math.LetterP CaseLower),
      ("P", MathLetter Math.LetterP CaseUpper),
      ("q", MathLetter Math.LetterQ CaseLower),
      ("Q", MathLetter Math.LetterQ CaseUpper),
      ("r", MathLetter Math.LetterR CaseLower),
      ("R", MathLetter Math.LetterR CaseUpper),
      ("s", MathLetter Math.LetterS CaseLower),
      ("S", MathLetter Math.LetterS CaseUpper),
      ("t", MathLetter Math.LetterT CaseLower),
      ("T", MathLetter Math.LetterT CaseUpper),
      ("u", MathLetter Math.LetterU CaseLower),
      ("U", MathLetter Math.LetterU CaseUpper),
      ("v", MathLetter Math.LetterV CaseLower),
      ("V", MathLetter Math.LetterV CaseUpper),
      ("w", MathLetter Math.LetterW CaseLower),
      ("W", MathLetter Math.LetterW CaseUpper),
      ("x", MathLetter Math.LetterX CaseLower),
      ("X", MathLetter Math.LetterX CaseUpper),
      ("y", MathLetter Math.LetterY CaseLower),
      ("Y", MathLetter Math.LetterY CaseUpper),
      ("z", MathLetter Math.LetterZ CaseLower),
      ("Z", MathLetter Math.LetterZ CaseUpper)
    ]

functionLut :: Vector (Text, MathSymbol)
functionLut =
  Vector.fromList
    [ ("sin", Math.FunctionS Math.SinF),
      ("cos", Math.FunctionS Math.CosF),
      ("tan", Math.FunctionS Math.TanF),
      ("sec", Math.FunctionS Math.SecF),
      ("csc", Math.FunctionS Math.CscF),
      ("cot", Math.FunctionS Math.CotF),
      ("arcsin", Math.FunctionS Math.ArcsinF),
      ("arccos", Math.FunctionS Math.ArccosF),
      ("arctan", Math.FunctionS Math.ArctanF),
      ("sinh", Math.FunctionS Math.SinhF),
      ("cosh", Math.FunctionS Math.CoshF),
      ("tanh", Math.FunctionS Math.TanhF),
      ("sech", Math.FunctionS Math.SechF),
      ("csch", Math.FunctionS Math.CschF),
      ("coth", Math.FunctionS Math.CothF),
      ("exp", Math.FunctionS Math.ExpF),
      ("log", Math.FunctionS Math.LogF),
      ("ln", Math.FunctionS Math.LnF),
      ("det", Math.FunctionS Math.DetF),
      ("dim", Math.FunctionS Math.DimF),
      ("mod", Math.FunctionS Math.ModF),
      ("gcd", Math.FunctionS Math.GcdF),
      ("lcm", Math.FunctionS Math.LcmF),
      ("lub", Math.FunctionS Math.LubF),
      ("glb", Math.FunctionS Math.GlbF),
      ("min", Math.FunctionS Math.MinF),
      ("max", Math.FunctionS Math.MaxF)
    ]
