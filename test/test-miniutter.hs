{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import qualified NLP.Miniutter.English as MU

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "check results of makePhrase calls"
    [ testMakePhraseNonRecursive ]
  ]

testMakePhraseNonRecursive :: Test
testMakePhraseNonRecursive = testGroup "non-recursive Part constructors"
  [ tp [MU.String "blue dog"]            "blue dog"
  , tp [MU.Text   "blue dog"]            "blue dog"
  , tp [MU.String "blue", MU.Text "dog"] "blue dog"
  , tp [MU.Cardinal 3]                   "three"
  , tp [MU.Cardinal 21]                  "21"
  , tp [MU.Cardinal (-3)]                "-3"
  , tp [MU.Cardinal 99999999999999992]   "99999999999999992"
  , tp [MU.Ordinal 3]                    "third"
  , tp [MU.Ordinal 21]                   "21st"
  , tp [MU.Ordinal (-3)]                 "-3rd"
  , tp [MU.Cardinal 99999999999999992]   "99999999999999992nd"
  ]
 where
  tp arg expect = testCase ("testPhrase " ++ show arg) $
    let obtain = MU.makePhrase arg
    in assertEqual (T.unpack expect ++ " == " ++ T.unpack obtain) expect obtain
