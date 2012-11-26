-- | Simple English clause creation parameterized by individual words.
module NLP.Miniutter.English
  ( Part(..), makeClause, makePhrase
  ) where

import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import NLP.Minimorph.English

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
  | QSubjectVerb Part Part    -- ^ question

-- | Realise a complete clause, capitalized, ending with a dot.
makeClause :: [Part] -> Text
makeClause l = capitalize $ makePhrase l `T.snoc` '.'

-- | Realise a fraction of a clause.
makePhrase :: [Part] -> Text
makePhrase = T.intercalate (T.singleton ' ') . map makePart

makePart :: Part -> Text
makePart _ = undefined

-- | Capitalize text.
capitalize :: Text -> Text
capitalize t = case T.uncons t of
  Nothing        -> T.empty
  Just (c, rest) -> T.cons (toUpper c) rest
