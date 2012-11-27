{-# LANGUAGE OverloadedStrings #-}
-- | Simple English clause creation parameterized by individual words.
module NLP.Miniutter.English
  ( Part(..), makeClause, makePhrase, defIrrp
  ) where

import Data.Char (toUpper)
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
  | W_sW Part Part      -- ^ possesive
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
  Ws p -> makePlural irrp (makePart irrp p)
  NWs n p -> T.pack (show n) <+> makePlural irrp (makePart irrp p)
  Ordinal n -> ordinal n
  NthW n p -> undefined  -- 1st
  AW p -> let t = makePart irrp p
          in indefiniteDet t <+> t
  WWandW lp -> let i = "and"
                   lt = makeParts irrp lp
               in commas i lt  -- TODO: generalize
  W_sW p_s p -> makePhrase irrp [p_s, p]  -- TODO
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
makePlural irrp t =
  case Map.lookup t irrp of
    Just u  -> u
    Nothing -> defaultNounPlural t

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
