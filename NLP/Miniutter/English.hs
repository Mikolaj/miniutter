{-# LANGUAGE OverloadedStrings #-}
-- | Simple English clause creation parameterized by individual words.
module NLP.Miniutter.English
  ( Part(..), makeClause, makePhrase, defIrrp
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
  | SubjectVerb Part Part     -- ^ requires conjugation
  | NotSubjectVerb Part Part  -- ^ negated
  | QSubjectVerb Part Part    -- ^ question; add question mark by hand
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
  NWs 1 p -> "1" <+> makePart irrp p
  NWs n p -> T.pack (show n) <+> onLastWord (makePlural irrp) (makePart irrp p)
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
  SubjectVerb s v -> makePhrase irrp [s, v]  -- TODO
  NotSubjectVerb s v -> makePhrase irrp [s, v]  -- TODO
  QSubjectVerb s v -> makePhrase irrp [v, s]  -- TODO

makeParts :: IrrPlural -> [Part] -> [Text]
makeParts irrp = filter (not . T.null) . map (makePart irrp)

-- | Capitalize text.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing        -> T.empty
  Just (c, rest) -> T.cons (toUpper c) rest

makePlural :: IrrPlural -> Text -> Text
makePlural _ "" = ""
makePlural irrp t =
  case Map.lookup t irrp of
    Just u  -> u
    Nothing -> defaultNounPlural t

-- TODO: move to minimorph; fix ordinal
-- | > ordinal 1 == "1st"
--   > ordinal 2 == "2nd"
--   > ordinal 3 == "3rd"
--   > ordinal 11 == "11th"
--   > ordinal 42 == "42nd"
ordinalNotSpelled :: Int -> Text
ordinalNotSpelled k = case abs $ k `rem` 100 of
  n | n > 3 && n < 21 -> k `suf` "th"
    | n `rem` 10 == 1 -> k `suf` "st"
    | n `rem` 10 == 2 -> k `suf` "nd"
    | n `rem` 10 == 3 -> k `suf` "rd"
    | otherwise       -> k `suf` "th"
 where
  num `suf` s = T.pack (show num) <> s

addIndefinite :: Text -> Text
addIndefinite "" = ""
addIndefinite t = indefiniteDet t <+> t

defaultPossesive :: Text -> Text
defaultPossesive "" = ""
defaultPossesive t =
  case T.last t of
    's'  -> t <> "'"
    'S'  -> t <> "'"
    '\'' -> t <> "s"
    _    -> t <> "'s"

onFirstWord :: (Text -> Text) -> Text -> Text
onFirstWord f t =
  let (starting, rest) = T.span isAlphaNum t
  in f starting <> rest

onLastWord :: (Text -> Text) -> Text -> Text
onLastWord f t =
  let (spanPrefix, spanRest) = T.span isAlphaNum $ T.reverse t
      (ending, rest) = (T.reverse spanPrefix, T.reverse spanRest)
  in rest <> f ending

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

-- | Default set of nouns with irregular plural forms.
defIrrp :: IrrPlural
defIrrp = Map.fromList
  [ ("canto",       "cantos")
  , ("homo ",       "homos")
  , ("photo",       "photos")
  , ("zero",        "zeros")
  , ("piano",       "pianos")
  , ("portico",     "porticos")
  , ("pro",         "pros")
  , ("quarto",      "quartos")
  , ("kimono",      "kimonos")
  , ("calf",        "calves")
  , ("leaf",        "leaves")
  , ("knife",       "knives")
  , ("life",        "lives")
  , ("dwarf",       "dwarfs")  -- not for ME dwarves, though
  , ("hoof",        "hooves")
  , ("elf",         "elves")
  , ("staff",       "staves")  -- depends on the meaning :<
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
