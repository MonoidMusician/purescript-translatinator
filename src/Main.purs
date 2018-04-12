module Main where

import Prelude

import CSS (gray)
import CSS as CSS
import Color (Color, rgb)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import DOM.HTML.Indexed (HTMLspan)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Aff (HalogenEffects, awaitLoad, runHalogenAff, selectElement)
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

data WordType = Verb | Adverb | Conjunction | Noun | Pronoun | Adjective | Particle
derive instance eqWordType :: Eq WordType
derive instance ordWordType :: Ord WordType
derive instance genericWordType :: Generic WordType _
instance showWordType :: Show WordType where show = genericShow
type Word = Tuple WordType String
data Punctuation = Comma | Newline | Space | Period | Colon
  | Enclitic String | Translation String
derive instance eqPunctuation :: Eq Punctuation
derive instance ordPunctuation :: Ord Punctuation
derive instance genericPunctuation :: Generic Punctuation _
instance showPunctuation :: Show Punctuation where show = genericShow
type Entity = Either Punctuation Word
type Codex = Array Entity
type Gloss = String
type Glossed = { glossed :: Word, gloss :: Gloss }
type Sample =
  { author :: String
  , content :: Codex
  , glosses :: Map String Gloss
  }

word :: Word -> Entity
word = Right

comma :: Entity
comma = Left Comma

period :: Entity
period = Left Period

newline :: Entity
newline = Left Newline

space :: Entity
space = Left Space

colon :: Entity
colon = Left Colon

_que :: Entity
_que = Left (Enclitic "que")

lit_ :: String -> Entity
lit_ = Left <<< Translation

verb :: String -> Word
verb = Tuple Verb

adverb :: String -> Word
adverb = Tuple Adverb

conjunction :: String -> Word
conjunction = Tuple Conjunction

noun :: String -> Word
noun = Tuple Noun

pronoun :: String -> Word
pronoun = Tuple Pronoun

adjective :: String -> Word
adjective = Tuple Adjective

particle :: String -> Word
particle = Tuple Particle

verb_ :: String -> Entity
verb_ = word <<< verb

adverb_ :: String -> Entity
adverb_ = word <<< adverb

conjunction_ :: String -> Entity
conjunction_ = word <<< conjunction

noun_ :: String -> Entity
noun_ = word <<< noun

pronoun_ :: String -> Entity
pronoun_ = word <<< pronoun

adjective_ :: String -> Entity
adjective_ = word <<< adjective

particle_ :: String -> Entity
particle_ = word <<< particle

{-
verb - red
adverb - orange
conjunction - yellow
noun - blue
pronoun - aqua
adjective - green ~cyan~
particle - purple

.verb {
  color: rgb(196, 53, 22);
}
.adverb {
  color: rgb(196, 53, 22);
}
.conjunction {
  color: rgb(218, 205, 26);
}
.noun {
  color: rgb(30, 31, 162);
}
.pronoun {
  color: rgb(28, 161, 190);
}
.relative.pronoun {
  color: rgb(171, 231, 42);
}
.adjective.pronoun {
  color: rgb(62, 200, 196);
}
.adjective {
  color: rgb(39, 212, 41);
}
.particle {
  color: rgb(190, 30, 148);
}
-}
colorType :: WordType -> Color
colorType = case _ of
  Verb -> rgb 196 53 22
  Adverb -> rgb 235 134 41
  Conjunction -> rgb 218 205 26
  Noun -> rgb 30 31 162
  Pronoun -> rgb 28 161 190
  Adjective -> rgb 39 212 41
  Particle -> rgb 190 30 148

colorize' :: forall o w. Array (HH.IProp HTMLspan o) -> Word -> HH.HTML w o
colorize' props (Tuple typ w) =
  HH.span (props <> [style (CSS.color (colorType typ))])
    [ HH.text w ]

colorize :: forall o w. Word -> HH.HTML w o
colorize = colorize' []

spacify' :: Codex -> { res :: Codex, allow_space :: Boolean }
spacify' = foldl folder { res: [], allow_space: false } where
  spaceIf = if _ then [space] else []
  classify = case _ of
    Newline -> Tuple false false
    Comma -> Tuple false true
    Period -> Tuple false true
    Space -> Tuple false false
    Colon -> Tuple false true
    Enclitic _ -> Tuple false true
    Translation _ -> Tuple true true
  folder { res, allow_space } (Right w) =
    { allow_space: true
    , res: res <> spaceIf allow_space <> [Right w]
    }
  folder { res, allow_space } (Left p) =
    let Tuple space_before space_after = classify p in
    { allow_space: space_after
    , res: res <> spaceIf (allow_space && space_before) <> [Left p]
    }
spacify :: Codex -> Codex
spacify = _.res <<< spacify'

punctuate :: forall o w. Punctuation -> HH.HTML w o
punctuate = case _ of
  Period -> HH.text "."
  Comma -> HH.text ","
  Space -> HH.text " "
  Newline -> HH.br_
  Colon -> HH.text ":"
  Enclitic c -> HH.span [style (CSS.color gray)] [ HH.text c ]
  Translation t -> HH.span [HP.class_ (wrap "translation")] [ HH.text t ]

{-
split :: Codex -> Array Codex
split =
-}

codex :: forall o w. Codex -> HH.HTML w o
codex caudex = HH.div_ $ spacify caudex <#> case _ of
  Left p -> punctuate p
  Right word -> colorize word

sample :: forall o w. Sample -> HH.HTML w (Maybe Glossed)
sample { author, content, glosses } = HH.section_
  [ HH.h2_ [ HH.text author ]
  , HH.div_ $ spacify content <#> case _ of
      Left p -> punctuate p
      Right word@(Tuple typ w) -> word # colorize'
        case Map.lookup w glosses of
          Nothing -> []
          Just gloss ->
            let glossed = { glossed: word, gloss } in
            [ HP.title gloss
            , HE.onMouseOver (pure $ pure $ pure glossed)
            , HE.onMouseOut (pure $ pure $ Nothing)
            ]
  ]

sample1 :: Sample
sample1 = { author: "Hrabanus Maurus", content, glosses } where
  content =
    [ noun_ "nūbibus", adjective_ "ātrīs", lit_ "dark clouds", newline
    , adjective_ "condita", adjective_ "nūllum", lit_ "(hidden) [no]", newline
    , verb_ "fundere", verb_ "possunt", lit_ "() are able to pour []", newline
    , noun_ "sīdera", noun_ "lūmen", lit_ "(stars) [light]", newline

    , conjunction_ "sī", noun_ "mare", verb_ "volvēns", newline
    , adjective_ "turbidus", noun_ "Auster", newline
    , verb_ "misceat", noun_ "aestum", comma, newline
    , adjective_ "vitrea", adverb_ "dūdum", newline
    , adverb_ "par", _que, adjective_ "serēnīs", newline
    , noun_ "unda", noun_ "diēbus", newline
    , adverb_ "mox", adjective_ "resolūtō", newline
    , adjective_ "sordida", noun_ "caenō", newline
    , noun_ "vīsibus", verb_ "obstat", comma, newline

    , pronoun_ "quīque", verb_ "vagātur", newline
    , noun_ "montibus", adjective_ "altīs", newline
    , adjective_ "dēfluus", noun_ "amnīs", newline
    , adverb_ "saepe", verb_ "restitit", newline
    , noun_ "rūpe", adjective_ "solūtī", newline
    , noun_ "ōbice", noun_ "saxī", period, newline

    , pronoun_ "tū", adverb_ "quoque", conjunction_ "sī", verb_ "vīs", newline
    , noun_ "lūmine", adjective_ "clārī", newline
    , verb_ "cernere", noun_ "vērum", newline
    , noun_ "trāmite", adjective_ "rēctō", newline
    , verb_ "carpere", noun_ "callem", colon, newline
    , noun_ "gaudia", verb_ "pelle", comma, newline
    , verb_ "pelle", noun_ "timōrem", newline
    , noun_ "spem", _que, verb_ "fugātō", newline
    , conjunction_ "nec", noun_ "dolor", verb_ "adsit", newline

    , adjective_ "nūbila", noun_ "mēns", verb_ "est", newline
    , adjective_ "vincta", _que, noun_ "frēnīs", newline
    , pronoun_ "haec", adverb_ "ubi", verb_ "regnant", period
    ]
  glosses = Map.fromFoldable
    [ Tuple "creāstī" "= creāvistī"
    ]

type State =
  { glossing :: Maybe Glossed
  }
data Query a
  = DoNothing a
  | Gloss Glossed a
  | Ungloss a
type ChildSlot = Void
type ChildrenQuery = Const Void

body :: forall eff m. MonadAff eff m =>
  H.Component HH.HTML Query Unit Void m
body = H.lifecycleParentComponent
  { eval, render
  , initialState: const { glossing: Nothing }
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  } where
    render :: State -> H.ParentHTML Query ChildrenQuery ChildSlot m
    render { glossing } =
      HH.div [ HP.id_ "parent" ]
        [ sidebar glossing, HH.div [] [ glosser <$> sample sample1 ] ]

    glosser = H.action <<< maybe Ungloss Gloss

    sidebar glossing = HH.div
      [ HP.id_ "sidebar"
      , style do
          -- CSS.backgroundColor purple
          CSS.width (30.0 # CSS.pct)
          -- CSS.height (100.0 # CSS.pct)
          CSS.float CSS.floatRight
      ]
      case glossing of
        Nothing -> [ HH.text "" ]
        Just { glossed, gloss } ->
          [ HH.h3_ [ colorize glossed ], HH.p_ [ HH.text gloss ] ]

    eval :: Query ~> H.ParentDSL State Query ChildrenQuery ChildSlot Void m
    eval (DoNothing a) = pure a
    eval (Gloss g a) = a <$ H.modify _ { glossing = Just g }
    eval (Ungloss a) = a <$ H.modify _ { glossing = Nothing }

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  awaitLoad
  selectElement (QuerySelector "#app") >>= traverse_ (runUI body unit)
