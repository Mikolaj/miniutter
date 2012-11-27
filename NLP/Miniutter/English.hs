-- | Simple English clause creation parameterized by individual words.
module NLP.Miniutter.English
  ( Part(..), makeClause, makePhrase
  ) where

import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import NLP.Minimorph.English
import NLP.Minimorph.Util

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

-- | Realise a complete clause, capitalized, ending with a dot.
makeClause :: [Part] -> Text
makeClause l = capitalize $ makePhrase l `T.snoc` '.'

-- | Realise a fraction of a clause.
makePhrase :: [Part] -> Text
makePhrase = T.intercalate (T.singleton ' ') . makeParts

makePart :: Part -> Text
makePart (String t) = T.pack t
makePart (Text t) = t
makePart (Cardinal n) = cardinal n
makePart (Ws p) = defaultNounPlural (makePart p)
makePart (NWs n p) = T.pack (show n) <+> defaultNounPlural (makePart p)
makePart (Ordinal n) = ordinal n
makePart (NthW n p) = undefined
makePart (AW p) =
  let t = makePart p
  in indefiniteDet t <> t
makePart (WWandW lp) =
  let i = T.pack "and"
      lt = makeParts lp
  in commas i lt  -- TODO: generalize
makePart (W_sW p_s p) = undefined
makePart (Compound p1 p2) = makePhrase [p1, p2]
makePart (SubjectVerb s v) = undefined
makePart (NotSubjectVerb s v) = undefined
makePart (QSubjectVerb s v) = undefined

makeParts :: [Part] -> [Text]
makeParts = filter (not . T.null) . map makePart

-- | Capitalize text.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing        -> T.empty
  Just (c, rest) -> T.cons (toUpper c) rest
