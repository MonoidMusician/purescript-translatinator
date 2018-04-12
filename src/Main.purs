module Main where

import Prelude

import CSS (gray)
import CSS as CSS
import Color (Color, rgb)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import DOM.HTML.Indexed (HTMLspan)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Array (mapWithIndex)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String as String
import Data.Tuple (Tuple(..), snd)
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
type Word =
  { word_type :: WordType
  , text :: String
  , href :: String
  , def :: String
  , alternate :: String
  , origin :: String
  , role :: String
  }
data Punctuation = Comma | Newline | Space | Period | Colon
  | Enclitic String | Translation String
derive instance eqPunctuation :: Eq Punctuation
derive instance ordPunctuation :: Ord Punctuation
derive instance genericPunctuation :: Generic Punctuation _
instance showPunctuation :: Show Punctuation where show = genericShow
type Entity = Either Punctuation Word
type Line = Array Entity
type Codex = Array Line
type Sample =
  { author :: String
  , work :: String
  , section :: String
  , content :: Codex
  , translation :: String
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

mkword :: WordType -> String -> Word
mkword word_type = { href: "", def: "", origin: "", alternate: "", role: "", word_type, text: _ }

addef :: Entity -> String -> Entity
addef w def = w <#> _ { def = def }
infixl 9 addef as @=

as :: Entity -> String -> Entity
as w role = w <#> _ { role = role }
infixl 9 as as @$

from :: Entity -> String -> Entity
from w origin = w <#> _ { origin = origin }
infixl 9 from as @<

verb :: String -> Word
verb = mkword Verb

adverb :: String -> Word
adverb = mkword Adverb

conjunction :: String -> Word
conjunction = mkword Conjunction

noun :: String -> Word
noun = mkword Noun

pronoun :: String -> Word
pronoun = mkword Pronoun

adjective :: String -> Word
adjective = mkword Adjective

particle :: String -> Word
particle = mkword Particle

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
colorize' props { word_type, text, role } =
  let
    couleur = CSS.color $ CSS.darken 0.05 $ colorType word_type
    klass = HP.class_ $ wrap role
  in HH.span (props <> [latin, klass, style couleur])
    [ HH.text text ]

latin :: forall i r. HP.IProp ( lang :: String | i ) r
latin = HP.prop (H.PropName "lang") "la"

colorize :: forall o w. Word -> HH.HTML w o
colorize = colorize' []

spacify' :: Line -> { res :: Line, allow_space :: Boolean }
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
spacify :: Line -> Line
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

nonempty :: forall a. String -> (String -> a) -> Array a
nonempty "" _ = []
nonempty v f = [f v]

split :: String -> Array String
split = String.trim >>> String.split (String.Pattern "\n")

sample :: forall w. Sample -> HH.HTML w (Tuple Boolean (Maybe Word))
sample { author, work, section, content, translation } = HH.section_
  [ HH.h2_ $ join [ [ HH.text (author <> ": " <> work) ], sec ]
  , HH.div [ HP.class_ (wrap "translation-parent") ] $ join
    [ map spacify content
      # mapWithIndex \row ->
        HH.p [HP.class_ (wrap "line"), atRow row] <<< map case _ of
          Left p -> punctuate p
          Right w -> w # colorize'
            [ HP.title w.role
            , HE.onClick (pure $ pure $ Tuple true $ pure w)
            , HE.onMouseOver (pure $ pure $ Tuple false $ pure w)
            , HE.onMouseOut (pure $ pure $ Tuple false $ Nothing)
            ]
    , split translation
      # mapWithIndex \row ->
        HH.p [atRow row] <<< pure <<< HH.text <<< (<>) " "
    ]
  ]
  where
    sec =
      nonempty section $ pure $ HH.h3 [ HP.class_ (wrap "section") ]
        [ HH.text ("(" <> section <> ")") ]
    atRow row = style (CSS.key (CSS.fromString "grid-row") (show (row+1)))

sample1 :: Sample
sample1 =
  { author: "Boëthius"
  , work: "Philosophy’s Consolation"
  , section: "Metron 1.7"
  , content, translation
  } where
  content =
    [ [ noun_ "nūbibus" @= "clouds", adjective_ "ātrīs" @= "dark" ]
    , [ adjective_ "condita" @= "hidden" @$ "nominative subject", adjective_ "nūllum" @= "no" @$ "accusative object" ]
    , [ verb_ "fundere" @= "to pour", verb_ "possunt" @= "are able" ]
    , [ noun_ "sīdera" @= "stars" @$ "nominative subject", noun_ "lūmen" @= "light" @$ "accusative object" ]

    , [ conjunction_ "sī" @= "if", noun_ "mare" @= "sea" @$ "accusative object", verb_ "volvēns" @= "rolling" @$ "active" ]
    , [ adjective_ "turbidus" @= "turbulent", noun_ "Auster" @= "the South Wind" ]
    , [ verb_ "misceat" @= "stir up", noun_ "aestum" @= "surge" @$ "accusative object", comma ]
    , [ adjective_ "vitrea" @= "glassy", adverb_ "dūdum" @= "just now" ]
    , [ adverb_ "par" @= "equally", _que, adjective_ "serēnīs" @= "tranquil" ]
    , [ noun_ "unda" @= "wave" @$ "nominative subject", noun_ "diēbus" @= "days" ]
    , [ adverb_ "mox" @= "soon", adjective_ "resolūtō" @= "loosened" ]
    , [ adjective_ "sordida" @= "foul", noun_ "caenō" @= "mud" @$ "ablative of means" ]
    , [ noun_ "vīsibus" @= "sight(s)", verb_ "obstat" @= "blocks", comma ]

    , [ pronoun_ "quīque" @= "whatever" @$ "nominative subject", verb_ "vagātur" @= "wanders" ]
    , [ noun_ "montibus" @= "mountains", adjective_ "altīs" @= "tall" ]
    , [ adjective_ "dēfluus" @= "flowing down" @$ "ablative of place from which", noun_ "amnis" @= "river" @$ "nominative subject" ]
    , [ adverb_ "saepe" @= "often", verb_ "restitit" @= "remains" ]
    , [ noun_ "rūpe" @= "cliff", adjective_ "solūtī" @= "loose" ]
    , [ noun_ "ōbice" @< "ōbex" @= "obstacle" @$ "ablative of place where", noun_ "saxī" @= "rock", period ]

    , [ pronoun_ "tū" @= "you", adverb_ "quoque" @= "also", conjunction_ "sī" @= "if", verb_ "vīs" @= "want" ]
    , [ noun_ "lūmine" @= "light", adjective_ "clārō" @= "clear" ]
    , [ verb_ "cernere" @= "discern", noun_ "vērum" @= "the truth" @$ "accusative object" ]
    , [ noun_ "trāmite" @= "riverbed", adjective_ "rēctō" @= "straight" ]
    , [ verb_ "carpere" @= "seize", noun_ "callem" @= "path" @$ "accusative object", colon ]
    , [ noun_ "gaudia" @= "joys" @$ "accusative object", verb_ "pelle" @= "drive away", comma ]
    , [ verb_ "pelle" @= "drive away", noun_ "timōrem" @= "fear" @$ "accusative object" ]
    , [ noun_ "spem" @= "hope", _que, verb_ "fugātō" @= "put to flight" ]
    , [ conjunction_ "nec" @= "nor", noun_ "dolor" @= "grief" @$ "nominative subject", verb_ "adsit" @= "be present" ]

    , [ adjective_ "nūbila" @= "cloudy" @$ "predicate", noun_ "mēns" @= "mind" @$ "nominative subject", verb_ "est" @= "is" ]
    , [ adjective_ "vincta" @= "bound" @$ "predicate", _que, noun_ "frēnīs" @= "bridle" @$ "ablative of instrument" ]
    , [ pronoun_ "haec" @= "these things" @$ "nominative subject", adverb_ "ubi" @= "when", verb_ "regnant" @= "reign", period ]
    ]
  translation = """
  Through black clouds
  the hidden stars
  can pour
  no light.
  If the turbulent South wind
  stirs up a surge,
  riling up the sea,
  the wave,
  just now glassy
  (as on calm days),
  soon foul with
  loosened mud
  blocks sight.
  Whatever river
  wanders flowing down
  the tall mountains
  often remains
  at an obstacle,
  a cliff of loose rock
  Tu also if you want
  to discern the truth
  in a clear light,
  to seize upon a path
  in the straight riverbed:
  throw aside joys,
  banish fear,
  and put hope to flight –
  let there be no grief!
  The mind is cloudy
  and bound by bridles
  when these things reign.
  """

type State =
  { glossing :: Maybe (Tuple Boolean Word)
  }
data Query a
  = DoNothing a
  | Gloss (Tuple Boolean (Maybe Word)) a
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
        [ sidebar (glossing <#> snd)
        , HH.div [] [ glosser <$> sample sample1 ]
        ]

    glosser = H.action <<< Gloss

    p = HH.p_ <<< pure <<< HH.text
    spla = HH.span [ latin ] <<< pure <<< HH.text

    sidebar glossing = HH.div
      [ HP.id_ "sidebar" ]
      case glossing of
        Nothing -> [ HH.text "" ]
        Just glossed -> join
          [ pure $ HH.h3_ [ colorize glossed ]
          , nonempty glossed.alternate (spla <<< append " = ")
          , nonempty glossed.origin (spla <<< append " < ")
          , nonempty glossed.def \v ->
              HH.p [HP.class_ (wrap "translation")]
                [ HH.text $ "“" <> v <> "”" ]
          , nonempty glossed.role p
          ]

    eval :: Query ~> H.ParentDSL State Query ChildrenQuery ChildSlot Void m
    eval (DoNothing a) = pure a
    eval (Gloss (Tuple clicked value) a) = a <$ do
      H.modify \r ->
        let
          glossing = case clicked, r.glossing of
            true, Just (Tuple true _) -> Nothing
            false, Just (Tuple true _) -> r.glossing
            _, _ -> value <#> Tuple clicked
        in r { glossing = glossing }

main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  awaitLoad
  selectElement (QuerySelector "#app") >>= traverse_ (runUI body unit)
