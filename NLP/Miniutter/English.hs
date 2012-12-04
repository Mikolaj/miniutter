{-# LANGUAGE OverloadedStrings #-}
-- | Simple English clause creation parameterized by individual words.
module NLP.Miniutter.English
  ( Part(..), makeClause, makePhrase, defIrrp, (<>), (<+>), showT
  ) where

import Data.Char (toUpper, isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import NLP.Minimorph.English
import NLP.Minimorph.Util
import Data.Map (Map)
import qualified Data.Map as Map

-- | Various basic and compound parts of English simple present tense clauses.
-- Many of the possible nestings do not make sense. We don't care.
data Part =
    String String       -- ^ handle for a String parameter
  | Text Text           -- ^ handle for a Text parameter
  | Cardinal Int        -- ^ cardinal number, spelled in full up to 10
  | Ws Part             -- ^ plural form of a word
  | NWs Int Part        -- ^ plural prefixed with a cardinal (not spelled)
  | Ordinal Int         -- ^ ordinal number, spelled in full up to 10
  | NthW Int Part       -- ^ word prefixed by an ordinal (not spelled)
  | AW Part             -- ^ word with indefinite article
  | WWandW [Part]       -- ^ enumeration
  | WWxW Part [Part]    -- ^ collection
  | Wown Part           -- ^ non-premodifying possesive
  | WownW Part Part     -- ^ attributive possesive
  | Compound Part Part  -- ^ separated with space, should very rarely be needed
  | NoSp Part Part      -- ^ no space in between
  | Part :> Text        -- ^ no space in between, a shorthand
  | Capitalize Part     -- ^ make the first letter into a capital letter
  | SubjectVerb Part Part     -- ^ singular conjugation (pronouns also plural)
  | NotSubjectVerb Part Part  -- ^ singular negated
  | QSubjectVerb Part Part    -- ^ singular question; add question mark by hand
  | SubjectVerbPlural Part Part     -- ^ plural conj. (pronouns also singular)
  | NotSubjectVerbPlural Part Part  -- ^ plural negated
  | QSubjectVerbPlural Part Part    -- ^ plural question
  deriving Show

-- | Nouns with irregular plural forms.
type IrrPlural = Map Text Text

-- | Realise a complete clause, capitalized, ending with a dot.
makeClause :: IrrPlural -> [Part] -> Text
makeClause irrp l = capitalize $ makePhrase irrp l `T.snoc` '.'

-- | Realise a fraction of a clause.
makePhrase :: IrrPlural -> [Part] -> Text
makePhrase irrp = T.intercalate (T.singleton ' ') . makeParts irrp

makePart :: IrrPlural -> Part -> Text
makePart irrp part = case part of
  String t -> T.pack t
  Text t -> t
  Cardinal n -> cardinal n
  Ws p -> onLastWord (makePlural irrp) (makePart irrp p)
  NWs 1 p -> makePart irrp (AW p)
  NWs n p -> showT n <+> onLastWord (makePlural irrp) (makePart irrp p)
  Ordinal n -> ordinal n
  NthW n p -> ordinalNotSpelled n <+> makePart irrp p
  AW p -> onFirstWord addIndefinite (makePart irrp p)
  WWandW lp -> let i = "and"
                   lt = makeParts irrp lp
               in commas i lt
  WWxW x lp -> let i = makePart irrp x
                   lt = makeParts irrp lp
               in commas i lt
  Wown p -> onLastWord nonPremodifying (makePart irrp p)
  WownW p1 p2 -> onLastWord attributive (makePart irrp p1) <+> makePart irrp p2
  Compound p1 p2 -> makePhrase irrp [p1, p2]
  NoSp p1 p2 -> makePart irrp p1 <> makePart irrp p2
  Capitalize p -> capitalize $ makePart irrp p
  p :> t -> makePart irrp p <> t
  SubjectVerb s v -> subjectVerb (makePart irrp s) (makePart irrp v)
  NotSubjectVerb s v -> notSubjectVerb (makePart irrp s) (makePart irrp v)
  QSubjectVerb s v -> qSubjectVerb (makePart irrp s) (makePart irrp v)
  SubjectVerbPlural s v ->
    subjectVerbPlural (makePart irrp s) (makePart irrp v)
  NotSubjectVerbPlural s v ->
    notSubjectVerbPlural (makePart irrp s) (makePart irrp v)
  QSubjectVerbPlural s v ->
    qSubjectVerbPlural (makePart irrp s) (makePart irrp v)

makeParts :: IrrPlural -> [Part] -> [Text]
makeParts irrp = filter (not . T.null) . map (makePart irrp)

-- | Capitalize text.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing        -> T.empty
  Just (c, rest) -> T.cons (toUpper c) rest

makePlural :: IrrPlural -> Text -> Text
makePlural irrp t =
  case Map.lookup t irrp of
    Just u  -> u
    Nothing -> defaultNounPlural t

addIndefinite :: Text -> Text
addIndefinite t = indefiniteDet t <+> t

disregardCase :: (Text -> Text -> a) -> Text -> Text -> a
disregardCase f s v =
  if s `elem` ["You", "He", "She", "It", "We", "They"]
  then f (T.toLower s) v
  else f s v

verbSingular :: Text -> Text -> Text
verbSingular "I"    "be" = "am"
verbSingular "you"  "be" = "are"
verbSingular "we"   "be" = "are"
verbSingular "they" "be" = "are"
verbSingular _      "be" = "is"
verbSingular "I"    v = v
verbSingular "you"  v = v
verbSingular "we"   v = v
verbSingular "they" v = v
verbSingular _ "have" = "has"
verbSingular _ "do"   = "does"
verbSingular _ "go"   = "goes"
verbSingular _ "can"    = "can"
verbSingular _ "could"  = "could"
verbSingular _ "must"   = "must"
verbSingular _ "will"   = "will"
verbSingular _ "would"  = "would"
verbSingular _ "shall"  = "shall"
verbSingular _ "should" = "should"
verbSingular _ "ought"  = "ought"
verbSingular _ "may"    = "may"
verbSingular _ "might"  = "might"
verbSingular _ "had"    = "had"
verbSingular _ v = fst (defaultVerbStuff v)

subjectVerb :: Text -> Text -> Text
subjectVerb s v =
  s <+> disregardCase (\s1 -> onFirstWord $ verbSingular s1) s v

notVerbSingular :: Text -> Text -> Text
notVerbSingular "I"    "be" = "am not"
notVerbSingular "you"  "be" = "aren't"
notVerbSingular "we"   "be" = "aren't"
notVerbSingular "they" "be" = "aren't"
notVerbSingular _      "be" = "isn't"
notVerbSingular _ "can"    = "can't"
notVerbSingular _ "could"  = "couldn't"
notVerbSingular _ "must"   = "mustn't"
notVerbSingular _ "will"   = "won't"
notVerbSingular _ "would"  = "wouldn't"
notVerbSingular _ "shall"  = "shan't"
notVerbSingular _ "should" = "shouldn't"
notVerbSingular _ "ought"  = "oughtn't"
notVerbSingular _ "may"    = "may not"
notVerbSingular _ "might"  = "might not"
notVerbSingular _ "had"    = "hadn't"
notVerbSingular "I"    v = "don't" <+> v
notVerbSingular "you"  v = "don't" <+> v
notVerbSingular "we"   v = "don't" <+> v
notVerbSingular "they" v = "don't" <+> v
notVerbSingular _ v = "doesn't" <+> v

notSubjectVerb :: Text -> Text -> Text
notSubjectVerb  s v =
  s <+> disregardCase (\s1 -> onFirstWord $ notVerbSingular s1) s v

qVerbSingular :: Text -> Text -> (Text, Text)
qVerbSingular "I"    "be" = ("am", "")
qVerbSingular "you"  "be" = ("are", "")
qVerbSingular "we"   "be" = ("are", "")
qVerbSingular "they" "be" = ("are", "")
qVerbSingular _      "be" = ("is", "")
qVerbSingular _ "can"    = ("can", "")
qVerbSingular _ "could"  = ("could", "")
qVerbSingular _ "must"   = ("must", "")
qVerbSingular _ "will"   = ("will", "")
qVerbSingular _ "would"  = ("would", "")
qVerbSingular _ "shall"  = ("shall", "")
qVerbSingular _ "should" = ("should", "")
qVerbSingular _ "ought"  = ("ought", "")
qVerbSingular _ "may"    = ("may", "")
qVerbSingular _ "might"  = ("might", "")
qVerbSingular _ "had"    = ("had", "")
qVerbSingular "I"    v = ("do", v)
qVerbSingular "you"  v = ("do", v)
qVerbSingular "we"   v = ("do", v)
qVerbSingular "they" v = ("do", v)
qVerbSingular _ v = ("does", v)

qSubjectVerb :: Text -> Text -> Text
qSubjectVerb s v =
  let (v1, v2) = disregardCase (\s1 -> onFirstWordPair $ qVerbSingular s1) s v
  in v1 <+> s <+> v2

verbPlural :: Text -> Text -> Text
verbPlural s@"I"   v = verbSingular s v
verbPlural s@"he"  v = verbSingular s v
verbPlural s@"she" v = verbSingular s v
verbPlural s@"it"  v = verbSingular s v
verbPlural _ "be" = "are"
verbPlural _ v = v

subjectVerbPlural :: Text -> Text -> Text
subjectVerbPlural s v =
  s <+> disregardCase (\s1 -> onFirstWord $ verbPlural s1) s v

notVerbPlural :: Text -> Text -> Text
notVerbPlural s@"I"   v = notVerbSingular s v
notVerbPlural s@"he"  v = notVerbSingular s v
notVerbPlural s@"she" v = notVerbSingular s v
notVerbPlural s@"it"  v = notVerbSingular s v
notVerbPlural _ "be" = "aren't"
notVerbPlural _ "can"    = "can't"
notVerbPlural _ "could"  = "couldn't"
notVerbPlural _ "must"   = "mustn't"
notVerbPlural _ "will"   = "won't"
notVerbPlural _ "would"  = "wouldn't"
notVerbPlural _ "shall"  = "shan't"
notVerbPlural _ "should" = "shouldn't"
notVerbPlural _ "ought"  = "oughtn't"
notVerbPlural _ "may"    = "may not"
notVerbPlural _ "might"  = "might not"
notVerbPlural _ "had"    = "hadn't"
notVerbPlural _ v = "don't" <+> v

notSubjectVerbPlural :: Text -> Text -> Text
notSubjectVerbPlural s v =
  s <+> disregardCase (\s1 -> onFirstWord $ notVerbPlural s1) s v

qVerbPlural :: Text -> Text -> (Text, Text)
qVerbPlural s@"I"   v = qVerbSingular s v
qVerbPlural s@"he"  v = qVerbSingular s v
qVerbPlural s@"she" v = qVerbSingular s v
qVerbPlural s@"it"  v = qVerbSingular s v
qVerbPlural _ "be" = ("are", "")
qVerbPlural _ "can"    = ("can", "")
qVerbPlural _ "could"  = ("could", "")
qVerbPlural _ "must"   = ("must", "")
qVerbPlural _ "will"   = ("will", "")
qVerbPlural _ "would"  = ("would", "")
qVerbPlural _ "shall"  = ("shall", "")
qVerbPlural _ "should" = ("should", "")
qVerbPlural _ "ought"  = ("ought", "")
qVerbPlural _ "may"    = ("may", "")
qVerbPlural _ "might"  = ("might", "")
qVerbPlural _ "had"    = ("had", "")
qVerbPlural _ v = ("do", v)

qSubjectVerbPlural :: Text -> Text -> Text
qSubjectVerbPlural s v =
  let (v1, v2) = disregardCase (\s1 -> onFirstWordPair $ qVerbPlural s1) s v
  in v1 <+> s <+> v2

isWordLetter :: Char -> Bool
isWordLetter c = isAlphaNum c || c == '\'' || c == '-'

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
-- | Default set of nouns with irregular plural forms.
defIrrp :: IrrPlural
defIrrp = Map.fromList
  [ ("canto",       "cantos")
  , ("homo",       "homos")
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
