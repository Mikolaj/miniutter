{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Simple English clause creation parameterized by individual words.
module NLP.Miniutter.English
  ( Part(..), Person(..), Polarity(..), Irregular
  , makeSentence, makePhrase, defIrregular, (<>), (<+>)
  ) where

import Data.Binary
import Data.Char (isAlphaNum, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import NLP.Minimorph.English
import NLP.Minimorph.Util hiding (showT, (<>))

-- | Various basic and compound parts of English simple present tense clauses.
-- Many of the possible nestings do not make sense. We don't care.
data Part =
    String !String      -- ^ handle for a String parameter
  | Text !Text          -- ^ handle for a Text parameter
  | Cardinal !Int       -- ^ cardinal number, spelled in full up to 10
  | Ws !Part            -- ^ plural form of a phrase
  | CarWs !Int !Part       -- ^ plural prefixed with a cardinal, not spelled
  | CardinalWs !Int !Part  -- ^ plural prefixed with a cardinal, spelled
  | Ordinal !Int        -- ^ ordinal number, spelled in full up to 10
  | Ord !Int            -- ^ ordinal number, not spelled
  | AW !Part            -- ^ phrase with indefinite article
  | WWandW ![Part]      -- ^ enumeration
  | WWxW !Part ![Part]  -- ^ collection
  | Wown !Part          -- ^ non-premodifying possesive
  | WownW !Part !Part   -- ^ attributive possesive
  | Append !Part !Part  -- ^ no space in between
  | !Part :> !Part      -- ^ no space in between  -- deprecated, use <>
  | Phrase ![Part]      -- ^ space-separated sequence
  | Capitalize !Part    -- ^ make the first letter into a capital letter
  | SubjectVerb !Person !Polarity !Part !Part
                        -- ^ conjugation according to polarity,
                        -- with a default person (pronouns override it)
  | SubjectVerbSg !Part !Part
                        -- ^ a shorthand for Sg3rd and Yes
  deriving (Show, Eq, Ord, Generic)

instance Binary Part

instance Read Part where
  readsPrec p str = [(Text x, y) | (x, y) <- readsPrec p str]

instance IsString Part where
  fromString = Text . T.pack

instance Monoid Part where
  mempty = Text ""
  mappend = Append

-- | Persons: singular 1st, singular 3rd and the rest.
data Person = Sg1st | Sg3rd | PlEtc
  deriving (Show, Eq, Ord, Generic)

instance Binary Person

-- | Generalized polarity: affirmative, negative, interrogative.
data Polarity = Yes | No | Why
  deriving (Show, Eq, Ord, Generic)

instance Binary Polarity

instance Binary Text where
   put = put . encodeUtf8
   get = decodeUtf8 `fmap` get

-- | Nouns with irregular plural form and nouns with irregular indefinite
-- article.
type Irregular = (Map Text Text, Map Text Text)

-- | Default set of words with irregular forms.
defIrregular :: Irregular
defIrregular = (defIrrPlural, defIrrIndefinite)

-- | Realise a complete sentence, capitalized, ending with a dot.
makeSentence :: Irregular -> [Part] -> Text
makeSentence irr l = capitalize $ makePhrase irr l `T.snoc` '.'

-- | Realise a phrase. The spacing between parts resembles
-- the semantics of @(\<\+\>)@, that is, it ignores empty words.
makePhrase :: Irregular -> [Part] -> Text
makePhrase irr = T.intercalate (T.singleton ' ') . makeParts irr

makeParts :: Irregular -> [Part] -> [Text]
makeParts irr = filter (not . T.null) . map (makePart irr)

-- The semantics of the operations is compositional.
makePart :: Irregular -> Part -> Text
makePart irr part = case part of
  String t -> T.pack t
  Text t -> t
  Cardinal n -> cardinal n
  Ws p -> onLastWord (makePlural irr) (mkPart p)
  CarWs 1 p -> mkPart (AW p)
  CarWs n p -> T.pack (show n) <+> onLastWord (makePlural irr) (mkPart p)
  CardinalWs 1 p -> mkPart (AW p)
  CardinalWs n p -> cardinal n <+> onLastWord (makePlural irr) (mkPart p)
  Ordinal n -> ordinal n
  Ord n -> ordinalNotSpelled n
  AW p -> onFirstWord (addIndefinite irr) (mkPart p)
  WWandW lp -> let i = "and"
                   lt = makeParts irr lp
               in commas i lt
  WWxW x lp -> let i = mkPart x
                   lt = makeParts irr lp
               in commas i lt
  Wown p -> onLastWord nonPremodifying (mkPart p)
  WownW p1 p2 -> onLastWord attributive (mkPart p1) <+> mkPart p2
  Phrase lp -> makePhrase irr lp
  Append p1 p2 -> mkPart p1 <> mkPart p2
  p1 :> p2 -> mkPart p1 <> mkPart p2
  Capitalize p -> capitalize $ mkPart p
  SubjectVerb defaultPerson Yes s v ->
    subjectVerb defaultPerson (mkPart s) (mkPart v)
  SubjectVerb defaultPerson No s v  ->
    notSubjectVerb defaultPerson (mkPart s) (mkPart v)
  SubjectVerb defaultPerson Why s v ->
    qSubjectVerb defaultPerson (mkPart s) (mkPart v)
  SubjectVerbSg s v          ->
    subjectVerb Sg3rd (mkPart s) (mkPart v)
 where
  mkPart = makePart irr

onFirstWord :: (Text -> Text) -> Text -> Text
onFirstWord f t =
  let (starting, rest) = T.span isWordLetter t
  in if T.null starting
     then rest
     else f starting <> rest

onLastWord :: (Text -> Text) -> Text -> Text
onLastWord f t =
  let (spanPrefix, spanRest) = T.span isWordLetter $ T.reverse t
      (ending, rest) = (T.reverse spanPrefix, T.reverse spanRest)
  in if T.null ending
     then rest
     else rest <> f ending

onFirstWordPair :: (Text -> (Text, Text)) -> Text -> (Text, Text)
onFirstWordPair f t =
  let (starting, rest) = T.span isWordLetter t
  in if T.null starting
     then (rest, "")
     else let (t1, t2) = f starting
          in (t1, t2 <> rest)

isWordLetter :: Char -> Bool
isWordLetter c = isAlphaNum c || c == '\'' || c == '-'

capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing        -> T.empty
  Just (c, rest) -> T.cons (toUpper c) rest

makePlural :: Irregular -> Text -> Text
makePlural (irrPlural, _) t =
  case Map.lookup t irrPlural of
    Just u  -> u
    Nothing -> defaultNounPlural t

addIndefinite :: Irregular -> Text -> Text
addIndefinite (_, irrIndefinite) t =
  case Map.lookup t irrIndefinite of
    Just u  -> u <+> t
    Nothing -> indefiniteDet t <+> t

guessPerson :: Person -> Text -> Person
guessPerson defaultPerson "i" = defaultPerson  -- letter 'i', not person 'I'
guessPerson defaultPerson word =
  case T.toLower word of
    "i"    -> Sg1st
    "he"   -> Sg3rd
    "she"  -> Sg3rd
    "it"   -> Sg3rd
    "we"   -> PlEtc
    "you"  -> PlEtc
    "they" -> PlEtc
    _      -> defaultPerson -- we don't try guessing beyond pronouns

personVerb :: Person -> Text -> Text
personVerb Sg1st "be" = "am"
personVerb PlEtc "be" = "are"
personVerb Sg3rd "be" = "is"
personVerb _ "can"    = "can"
personVerb _ "could"  = "could"
personVerb _ "must"   = "must"
personVerb _ "will"   = "will"
personVerb _ "would"  = "would"
personVerb _ "shall"  = "shall"
personVerb _ "should" = "should"
personVerb _ "ought"  = "ought"
personVerb _ "may"    = "may"
personVerb _ "might"  = "might"
personVerb _ "had"    = "had"
personVerb Sg1st v = v
personVerb PlEtc v = v
personVerb Sg3rd "have" = "has"
personVerb Sg3rd v = fst (defaultVerbStuff v)

subjectVerb :: Person -> Text -> Text -> Text
subjectVerb defaultPerson s v =
  s <+> onFirstWord (personVerb $ guessPerson defaultPerson s) v

notPersonVerb :: Person -> Text -> Text
notPersonVerb Sg1st "be" = "am not"
notPersonVerb PlEtc "be" = "aren't"
notPersonVerb Sg3rd "be" = "isn't"
notPersonVerb _ "can"    = "can't"
notPersonVerb _ "could"  = "couldn't"
notPersonVerb _ "must"   = "mustn't"
notPersonVerb _ "will"   = "won't"
notPersonVerb _ "would"  = "wouldn't"
notPersonVerb _ "shall"  = "shan't"
notPersonVerb _ "should" = "shouldn't"
notPersonVerb _ "ought"  = "oughtn't"
notPersonVerb _ "may"    = "may not"
notPersonVerb _ "might"  = "might not"
notPersonVerb _ "had"    = "hadn't"
notPersonVerb Sg1st v = "don't" <+> v
notPersonVerb PlEtc v = "don't" <+> v
notPersonVerb Sg3rd v = "doesn't" <+> v

notSubjectVerb :: Person -> Text -> Text -> Text
notSubjectVerb defaultPerson s v =
  s <+> onFirstWord (notPersonVerb $ guessPerson defaultPerson s) v

qPersonVerb :: Person -> Text -> (Text, Text)
qPersonVerb Sg1st "be" = ("am", "")
qPersonVerb PlEtc "be" = ("are", "")
qPersonVerb Sg3rd "be" = ("is", "")
qPersonVerb _ "can"    = ("can", "")
qPersonVerb _ "could"  = ("could", "")
qPersonVerb _ "must"   = ("must", "")
qPersonVerb _ "will"   = ("will", "")
qPersonVerb _ "would"  = ("would", "")
qPersonVerb _ "shall"  = ("shall", "")
qPersonVerb _ "should" = ("should", "")
qPersonVerb _ "ought"  = ("ought", "")
qPersonVerb _ "may"    = ("may", "")
qPersonVerb _ "might"  = ("might", "")
qPersonVerb _ "had"    = ("had", "")
qPersonVerb Sg1st v = ("do", v)
qPersonVerb PlEtc v = ("do", v)
qPersonVerb Sg3rd v = ("does", v)

qSubjectVerb :: Person -> Text -> Text -> Text
qSubjectVerb defaultPerson s v =
  let (v1, v2) = onFirstWordPair (qPersonVerb $ guessPerson defaultPerson s) v
  in v1 <+> s <+> v2

nonPremodifying :: Text -> Text
nonPremodifying "who"  = "whose"
nonPremodifying "Who"  = "Whose"
nonPremodifying "I"    = "mine"
nonPremodifying "you"  = "yours"
nonPremodifying "You"  = "Yours"
nonPremodifying "he"   = "his"
nonPremodifying "He"   = "His"
nonPremodifying "she"  = "her"
nonPremodifying "She"  = "Her"
nonPremodifying "it"   = "its"
nonPremodifying "It"   = "Its"
nonPremodifying "we"   = "ours"
nonPremodifying "We"   = "Ours"
nonPremodifying "they" = "theirs"
nonPremodifying "They" = "Theirs"
nonPremodifying t = defaultPossesive t

attributive :: Text -> Text
attributive "who"  = "whose"
attributive "Who"  = "Whose"
attributive "I"    = "my"
attributive "you"  = "your"
attributive "You"  = "Your"
attributive "he"   = "his"
attributive "He"   = "His"
attributive "she"  = "her"
attributive "She"  = "Her"
attributive "it"   = "its"
attributive "It"   = "Its"
attributive "we"   = "our"
attributive "We"   = "Our"
attributive "they" = "their"
attributive "They" = "Their"
attributive t = defaultPossesive t

-- TODO: use a suffix tree, to catch ableman, seaman, etc.?
-- | Default set of nouns with irregular plural form.
defIrrPlural :: Map Text Text
defIrrPlural = Map.fromList
  [ ("canto",       "cantos")
  , ("homo",        "homos")
  , ("photo",       "photos")
  , ("zero",        "zeros")
  , ("piano",       "pianos")
  , ("portico",     "porticos")
  , ("pro",         "pros")
  , ("quarto",      "quartos")
  , ("kimono",      "kimonos")
  , ("knife",       "knives")
  , ("life",        "lives")
  , ("dwarf",       "dwarfs")  -- not for ME dwarves, though
  , ("proof",       "proofs")
  , ("roof",        "roofs")
  , ("turf",        "turfs")
  , ("child",       "children")
  , ("foot",        "feet")
  , ("goose",       "geese")
  , ("louse",       "lice")
  , ("man",         "men")
  , ("mouse",       "mice")
  , ("tooth",       "teeth")
  , ("woman",       "women")
  , ("buffalo",     "buffalo")
  , ("deer",        "deer")
  , ("moose",       "moose")
  , ("sheep",       "sheep")
  , ("bison",       "bison")
  , ("salmon",      "salmon")
  , ("pike",        "pike")
  , ("trout",       "trout")
  , ("swine",       "swine")
  , ("aircraft",    "aircraft")
  , ("watercraft",  "watercraft")
  , ("spacecraft",  "spacecraft")
  , ("hovercraft",  "hovercraft")
  , ("information", "information")
  ]

-- TODO: Remove the exceptions about 'u' as soon as we bump the minimorph
-- dependency to 0.1.5.
-- | Default set of nouns with irregular indefinite article.
defIrrIndefinite :: Map Text Text
defIrrIndefinite = Map.fromList
  [ ("user",         "a")
  , ("university",   "a")
  , ("unicorn",      "a")
  , ("unicycle",     "a")
  , ("usual",        "a")
  , ("unique",       "a")
  , ("uniform",      "a")
  , ("SCUBA",        "a")
  , ("HEPA",         "a")
  , ("hour",         "an")
  , ("heir",         "an")
  , ("honour",       "an")
  , ("honor",        "an")
  ]
