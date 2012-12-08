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
    [ testMakePhraseVerbatim
    , testMakePhrasePlural
    , testMakePhraseNumber
    , testMakePhraseIndefinite
    , testMakePhraseEnumeration
    , testMakePhrasePossesive
    , testMakePhraseSubjectVerb
    , testAllureOfTheStars
    ]
  ]

tp :: [MU.Part] -> T.Text -> Test
tp arg expect =
  testCase ("testPhrase " ++ show arg)
  $ let obtain = MU.makePhrase MU.defIrregular arg
    in assertEqual (T.unpack expect ++ " == " ++ T.unpack obtain) expect obtain

testMakePhraseVerbatim :: Test
testMakePhraseVerbatim = testGroup "verbatim text Part constructors"
  [ tp [MU.String "blue dog"]              "blue dog"
  , tp [MU.Text   "blue dog"]              "blue dog"
  , tp [MU.String ""]                      ""
  , tp [MU.Text   ""]                      ""
  , tp [MU.String "blue", MU.String "dog"] "blue dog"
  , tp [MU.String "blue", MU.String ""]    "blue"
  , tp [MU.String "", MU.String "dog"]     "dog"
  , tp [MU.String "", MU.String ""]        ""
  , tp [MU.String "blue", "dog"]           "blue dog"
  , tp [MU.String "blue", ""]              "blue"
  , tp [MU.String "", "dog"]               "dog"
  , tp [MU.String "", ""]                  ""
  , tp ["blue", MU.String "dog"]           "blue dog"
  , tp ["blue", MU.String ""]              "blue"
  , tp ["", MU.String "dog"]               "dog"
  , tp ["", MU.String ""]                  ""
  , tp ["blue", "dog"]                     "blue dog"
  , tp ["blue", ""]                        "blue"
  , tp ["", "dog"]                         "dog"
  , tp ["", ""]                            ""
  , tp [MU.Phrase ["blue", "dog"]]         "blue dog"
  , tp [MU.Phrase ["blue", ""]]            "blue"
  , tp [MU.Phrase ["", "dog"]]             "dog"
  , tp [MU.Phrase ["", ""]]                ""
  , tp [MU.Ordinal 0 MU.:> ", but", MU.Ordinal 1] "0th, but first"
  , tp [MU.Cardinal 20 MU.:> MU.Cardinal 0]       "200"
  , tp [MU.String " "]                            " "
  , tp [  " "]                                    " "
  , tp [MU.String " blue ", MU.String " dog "]    " blue   dog "
  , tp [" blue ", " dog "]                        " blue   dog "
  , tp [MU.Phrase [" blue ", " dog "]]            " blue   dog "
  , testCase "testPhrase makeSentence and Capitalize" $
      assertEqual "makeSentence == Capitalize makePhrase :> '.'"
        (MU.makePhrase MU.defIrregular
           [MU.Capitalize (MU.SubjectVerbSg "goblin" "hit") MU.:> "."])
        (MU.makeSentence MU.defIrregular
           [MU.SubjectVerbSg "goblin" "hit"])
  ]

testMakePhrasePlural :: Test
testMakePhrasePlural = testGroup "plural form Part constructors"
  [ tp [MU.Ws "dog"]        "dogs"
  , tp [MU.Ws "dogs"]       "dogses"
  , tp [MU.Ws "blue dog"]   "blue dogs"
  , tp [MU.Ws "blue dog  "] "blue dog  "
  , tp [MU.Ws "blue dog."]  "blue dog."
  , tp [MU.Ws "blue dog%"]  "blue dog%"
  , tp [MU.Ws "bitch"]      "bitches"
  , tp [MU.Ws "the fish"]   "the fishes"
  , tp [MU.Ws "miss"]       "misses"
  , tp [MU.Ws "buzz"]       "buzzes"
  , tp [MU.Ws "box"]        "boxes"
  , tp [MU.Ws "hajj"]       "hajjes"
  , tp [MU.Ws "goto"]       "gotoes"
  , tp [MU.Ws "igloo"]      "igloos"
  , tp [MU.Ws "buy"]        "buys"
  , tp [MU.Ws "try"]        "tries"
  , tp [MU.Ws "canto"]      "cantos"
  , tp [MU.Ws "homo"]       "homos"
  , tp [MU.Ws "photo"]      "photos"
  , tp [MU.Ws "nice zero"]  "nice zeros"
  , tp [MU.Ws "piano"]      "pianos"
  , tp [MU.Ws "portico"]    "porticos"
  , tp [MU.Ws "pro"]        "pros"
  , tp [MU.Ws "quarto"]     "quartos"
  , tp [MU.Ws "kimono"]     "kimonos"
  , tp [MU.Ws "calf"]       "calves"
  , tp [MU.Ws "leaf"]       "leaves"
  , tp [MU.Ws "knife"]      "knives"
  , tp [MU.Ws "life"]       "lives"
  , tp [MU.Ws "dwarf"]      "dwarfs"
  , tp [MU.Ws "hoof"]       "hooves"
  , tp [MU.Ws "elf"]        "elves"
--, tp [MU.Ws "staff"]      "staves"  -- depends on the meaning :(
  , tp [MU.Ws "child"]      "children"
  , tp [MU.Ws "foot"]       "feet"
  , tp [MU.Ws "goose"]      "geese"
  , tp [MU.Ws "louse"]      "lice"
  , tp [MU.Ws "man"]        "men"
  , tp [MU.Ws "mouse"]      "mice"
  , tp [MU.Ws "tooth"]      "teeth"
  , tp [MU.Ws "woman"]      "women"
  , tp [MU.Ws "buffalo"]    "buffalo"
  , tp [MU.Ws "deer"]       "deer"
  , tp [MU.Ws "the moose"]  "the moose"
  , tp [MU.Ws "sheep"]      "sheep"
  , tp [MU.Ws "bison"]      "bison"
  , tp [MU.Ws "salmon"]     "salmon"
  , tp [MU.Ws "pike"]       "pike"
  , tp [MU.Ws "trout"]      "trout"
  , tp [MU.Ws "swine"]      "swine"
  , tp [MU.Ws "aircraft"]   "aircraft"
  , tp [MU.Ws "watercraft"] "watercraft"
  , tp [MU.Ws "spacecraft"] "spacecraft"
  , tp [MU.Ws "hovercraft"] "hovercraft"
  , tp [MU.Ws "information"]                  "information"
  , tp [MU.Ws (MU.String "dog blue")]         "dog blues"
  , tp [MU.Ws (MU.Ordinal 1)]                 "firsts"
  , tp [MU.Ws (MU.Ws "do")]                   "doeses"
  , tp [MU.Ws (MU.NWs 1 "man")]               "a men"
  , tp [MU.Ws (MU.NthW 1 "man")]              "1st men"
  , tp [MU.Ws (MU.AW "elf")]                  "an elves"
  , tp [MU.Ws (MU.WWandW ["dog", "eagle", "parrot"])]
                                              "dog, eagle and parrots"
  , tp [MU.Ws (MU.WWxW "and also" ["dog", "eagle", "parrot"])]
                                              "dog, eagle and also parrots"
  , tp [MU.Ws (MU.Wown "uncle")]              "uncle'ses"
  , tp [MU.Ws (MU.WownW "uncle" "dog")]       "uncle's dogs"
  , tp [MU.Ws (MU.Phrase ["uncle", "dog"])]   "uncle dogs"
  , tp [MU.Ws (MU.SubjectVerb MU.Sg3rd MU.Yes "I" "do")]     "I does"
  , tp [MU.Ws (MU.SubjectVerb MU.Sg1st MU.No "Me" "do")]      "Me don't does"
  , tp [MU.Ws (MU.SubjectVerb MU.Sg3rd MU.Why "woman" "do")] "does woman does"
  ]

testMakePhraseNumber :: Test
testMakePhraseNumber = testGroup "number Part constructors"
  [ tp [MU.Cardinal 3]                 "three"
  , tp [MU.Cardinal 1111]              "1111"
  , tp [MU.Cardinal 131]               "131"
  , tp [MU.Cardinal 131, MU.Ordinal 2] "131 second"
  , tp [MU.Cardinal (-3)]              "-3"
  , tp [MU.Cardinal 99999999999999992] "99999999999999992"
  , tp [MU.Ordinal 3]                  "third"
  , tp [MU.Ordinal 1111]               "1111th"
  , tp [MU.Ordinal 131]                "131st"
  , tp [MU.Ordinal 131, MU.Cardinal 2] "131st two"
  , tp [MU.Ordinal (-3)]               "-3rd"
  , tp [MU.Ordinal 99999999999999992]  "99999999999999992nd"
  , tp [MU.NWs 1 "blue dog"]           "a blue dog"
  , tp [MU.NWs 2 "blue elf"]           "2 blue elves"
  , tp [MU.NWs 2 " dog "]              "2  dog "
  , tp [MU.NWs 3 "leaf"]               "3 leaves"
  , tp [MU.NWs 4 "sheep"]              "4 sheep"
  , tp [MU.NWs (-1) "dog"]             "-1 dogs"
  , tp [MU.NWs (-3) "dog"]             "-3 dogs"
  , tp [MU.NWs 12 ""]                  "12"
  , tp [MU.NWs 5 (MU.Cardinal 1)]      "5 ones"
  , tp [MU.NWs 4 (MU.Ordinal 2)]       "4 seconds"
  , tp [MU.NthW 2 "blue dog"]          "2nd blue dog"
  , tp [MU.NthW 2 " dog "]             "2nd  dog "
  , tp [MU.NthW 3 "leaf"]              "3rd leaf"
  , tp [MU.NthW 4 "sheep"]             "4th sheep"
  , tp [MU.NthW (-3) "dog"]            "-3rd dog"
  , tp [MU.NthW 12 ""]                 "12th"
  , tp [MU.NthW 51 ""]                 "51st"
  , tp [MU.NthW 52 ""]                 "52nd"
  , tp [MU.NthW 951 ""]                "951st"
  , tp [MU.NthW 952 ""]                "952nd"
  , tp [MU.NthW 112 ""]                "112th"
  , tp [MU.NthW 712 ""]                "712th"
  , tp [MU.NthW 5 (MU.Cardinal 1)]     "5th one"
  , tp [MU.NthW 4 (MU.Ordinal 2)]      "4th second"
  , tp [MU.NthW 4 (MU.NthW 7 "dog")]   "4th 7th dog"
  , tp [MU.NthW 4 (MU.NWs 7 "dog")]    "4th 7 dogs"
  , tp [MU.NWs 4 (MU.NWs 7 "dog")]     "4 7 dogses"
  , tp [MU.NWs 4 (MU.NthW 7 "elf")]    "4 7th elves"
  ]

testMakePhraseIndefinite :: Test
testMakePhraseIndefinite = testGroup "indefinite article"
  [ tp [MU.AW "user"]             "a user"
  , tp [MU.AW "usual egg"]        "a usual egg"
  , tp [MU.AW "ABC"]              "an ABC"
  , tp [MU.AW " ABC"]             " ABC"
  , tp [MU.AW "ABC "]             "an ABC "
  , tp [MU.AW "SCUBA"]            "a SCUBA"
  , tp [MU.AW "SSI"]              "an SSI"
  , tp [MU.AW (MU.String "yell")] "a yell"
  , tp [MU.AW (MU.Cardinal 3)]    "a three"
  , tp [MU.AW (MU.Cardinal 8)]    "an eight"
  , tp [MU.AW (MU.Cardinal 31)]   "a 31"
  , tp [MU.AW (MU.Cardinal 83)]   "an 83"
  , tp [MU.AW (MU.NWs 3 "dog")]   "a 3 dogs"
  , tp [MU.AW (MU.Ordinal 3)]     "a third"
  , tp [MU.AW (MU.Ordinal 8)]     "an eighth"
  , tp [MU.AW (MU.Ordinal 31)]    "a 31st"
  , tp [MU.AW (MU.Ordinal 83)]    "an 83rd"
  , tp [MU.AW (MU.NthW 3 "dog")]  "a 3rd dog"
  , tp [MU.AW (MU.AW "dog")]      "an a dog"
  , tp [MU.AW (MU.WWandW ["dog", "eagle", "parrot"])]
                                            "a dog, eagle and parrot"
  , tp [MU.AW (MU.WWxW "or otherwise" ["hour", "eagle", "car"])]
                                            "an hour, eagle or otherwise car"
  , tp [MU.AW (MU.Wown "uncle")]            "an uncle's"
  , tp [MU.AW (MU.WownW "uncle" "dog")]     "an uncle's dog"
  , tp [MU.AW (MU.Phrase ["uncle", "dog"])] "an uncle dog"
  , tp [MU.AW (MU.SubjectVerbSg "I" "do")]  "an I do"
  , tp [MU.AW (MU.SubjectVerb MU.Sg3rd MU.No "I" "do")]  "an I don't do"
  , tp [MU.AW (MU.SubjectVerb MU.Sg3rd MU.Why "I" "do")] "a do I do"
  ]

testMakePhraseEnumeration :: Test
testMakePhraseEnumeration = testGroup "enumeration and collection"
  [ tp [MU.WWandW [MU.String "dog", "eagle", MU.Cardinal 7]]
                                          "dog, eagle and seven"
  , tp [MU.WWxW "then" [MU.Ordinal 113, MU.AW (MU.String "eagle"), "parrot"]]
                                          "113th, an eagle then parrot"
  , tp [MU.WWandW [ MU.String "I"
                  , MU.WWandW [MU.String "I", MU.Ordinal 31, MU.Cardinal 17]
                  , MU.WWandW ["I", MU.AW (MU.Ordinal 18)]
                  ]]
                                          "I, I, 31st and 17 and I and an 18th"
  , tp [MU.WWxW " and perhaps%" [" dog ", "", "%eagle."]]
                                          " dog   and perhaps% %eagle."
  ]

testMakePhrasePossesive :: Test
testMakePhrasePossesive = testGroup "the possesive form"
  [ tp [MU.Wown (MU.String "uncle")]   "uncle's"
  , tp [MU.Wown (MU.String " uncle ")] " uncle "
  , tp [MU.Wown ""]                    ""
  , tp [MU.Wown " "]                   " "
  , tp [MU.Wown "miss"]                "miss'"
  , tp [MU.Wown "YQS"]                 "YQS'"
  , tp [MU.Wown "buzz"]                "buzz's"
  , tp [MU.Wown "box"]                 "box's"
  , tp [MU.Wown "Who"]                 "Whose"
  , tp [MU.Wown "I"]                   "mine"
  , tp [MU.Wown "you"]                 "yours"
  , tp [MU.Wown "he"]                  "his"
  , tp [MU.Wown "She"]                 "Her"
  , tp [MU.Wown "it"]                  "its"
  , tp [MU.Wown "We"]                  "Ours"
  , tp [MU.Wown "they"]                "theirs"
  , tp [MU.WownW (MU.String "uncle") (MU.String "dog")] "uncle's dog"
  , tp [MU.WownW " uncle " "dog"]                       " uncle  dog"
  , tp [MU.WownW "I" ""]                                "my"
  , tp [MU.WownW "" "dog"]                              "dog"
  , tp [MU.WownW "" ""]                                 ""
  , tp [MU.WownW " " " "]                               "   "
  , tp [MU.WownW "miss" "dog"]                          "miss' dog"
  , tp [MU.WownW "YQS" (MU.Cardinal 33)]                "YQS' 33"
  , tp [MU.WownW "buzz" (MU.Ordinal 21)]                "buzz's 21st"
  , tp [MU.WownW "box" ""]                              "box's"
  , tp [MU.WownW "who" "dog"]                           "whose dog"
  , tp [MU.WownW "I" "dog"]                             "my dog"
  , tp [MU.WownW "you" "dog"]                           "your dog"
  , tp [MU.WownW "He" "dog"]                            "His dog"
  , tp [MU.WownW "she" "dog"]                           "her dog"
  , tp [MU.WownW "It" "dog"]                            "Its dog"
  , tp [MU.WownW "we" "dog"]                            "our dog"
  , tp [MU.WownW "They" "dog"]                          "Their dog"
  , tp [MU.Wown (MU.NWs 6 "")]                          "6's"
  , tp [MU.Wown (MU.NthW 1 "")]                         "1st's"
  , tp [MU.Wown (MU.Ws (MU.NWs 6 ""))]                  "6s'"
  , tp [MU.Wown (MU.WWandW ["I", "you"])]               "I and yours"
  , tp [MU.Wown (MU.WWandW ["you", "I"])]               "you and mine"
  , tp [MU.WownW (MU.WWandW ["you", "I"]) "dog"]        "you and my dog"
  , tp [MU.Wown (MU.Wown "it")]                         "its'"
  , tp [MU.Wown $ MU.Wown $ MU.Wown $ MU.Wown $ "it"]   "its's'"
  , tp [MU.Wown (MU.SubjectVerb MU.Sg3rd MU.Why "I" "be")] "am mine"
  , tp [MU.Wown " do   I"]                              " do   mine"
  , tp [MU.Wown " do   I "]                             " do   I "
  ]

testMakePhraseSubjectVerb :: Test
testMakePhraseSubjectVerb = testGroup "subject and verb"
  [ tp [MU.SubjectVerbSg "species" "look"]               "species looks"
  , tp [MU.SubjectVerb MU.Sg1st MU.No "species" "look"]  "species don't look"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "species" "look"] "does species look"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "species" "look"] "species look"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "species" "look"]  "species don't look"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "species" "look"] "do species look"
  , tp [MU.SubjectVerbSg "I" "be"]                       "I am"
  , tp [MU.SubjectVerb MU.Sg3rd MU.No "you" "be"]        "you aren't"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "she" "be"]       "is she"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "we" "be"]        "we are"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "I" "be"]          "I am not"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "they" "be"]      "are they"
  , tp [MU.SubjectVerbSg "they" "be"]                    "they are"
  , tp [MU.SubjectVerb MU.Sg3rd MU.No "we" "be"]         "we aren't"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "it" "be"]        "is it"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "he" "be"]        "he is"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "She" "be"]        "She isn't"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "You" "be"]       "are You"
  , tp [MU.SubjectVerbSg "Tom" "have"]                   "Tom has"
  , tp [MU.SubjectVerb MU.Sg3rd MU.No "cat" "have"]      "cat doesn't have"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "they" "have"]    "do they have"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "he" "have"]      "he has"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "She" "have"]      "She doesn't have"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "Foos" "have"]    "do Foos have"
  , tp [MU.SubjectVerbSg "Tom" "do"]                     "Tom does"
  , tp [MU.SubjectVerb MU.Sg3rd MU.No "cat" "do"]        "cat doesn't do"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "they" "do"]      "do they do"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "he" "go"]        "he goes"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "She" "go"]        "She doesn't go"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "Foos" "go"]      "do Foos go"
  , tp [MU.SubjectVerbSg "Tom" "can"]                    "Tom can"
  , tp [MU.SubjectVerb MU.Sg3rd MU.No "cat" "could"]     "cat couldn't"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "they" "must"]    "must they"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "he" "will"]      "he will"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "She" "would"]     "She wouldn't"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "Foos" "shall"]   "shall Foos"
  , tp [MU.SubjectVerbSg "Tom" "should"]                 "Tom should"
  , tp [MU.SubjectVerb MU.Sg3rd MU.No "cat" "ought"]     "cat oughtn't"
  , tp [MU.SubjectVerb MU.Sg3rd MU.Why "they" "may"]     "may they"
  , tp [MU.SubjectVerb MU.PlEtc MU.Yes "he" "might"]     "he might"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "She" "had"]       "She hadn't"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "it" "copy down"] "does it copy down"
  , tp [MU.SubjectVerbSg "Tom" "copy down"       ]       "Tom copies down"
  , tp [MU.SubjectVerbSg "Tom" "buzz"]                   "Tom buzzes"
  , tp [MU.SubjectVerbSg "Tom" "it it"]                  "Tom its it"
  , tp [MU.SubjectVerbSg "Tom" "you you"]                "Tom yous you"
  , tp [MU.SubjectVerbSg "You" "you you"]                "You you you"
  , tp [MU.SubjectVerbSg "She" "do read"]                "She does read"
  , tp [MU.SubjectVerbSg "She" "do do"]                  "She does do"
  , tp [MU.SubjectVerb MU.PlEtc MU.Why "she" "do"]       "does she do"
  , tp [MU.SubjectVerb MU.PlEtc MU.No "She" "had had"]   "She hadn't had"
  ]

tc :: [MU.Part] -> T.Text -> Test
tc arg expect =
  testCase ("testSentence " ++ show arg)
  $ let obtain = MU.makeSentence MU.defIrregular arg
    in assertEqual (T.unpack expect ++ " == " ++ T.unpack obtain) expect obtain

testAllureOfTheStars:: Test
testAllureOfTheStars = testGroup "Allure of the Stars utterances"
  [ tc [ MU.SubjectVerbSg "you" "displace"
       , "Haskell Alvin" ]
       "You displace Haskell Alvin."
  , tc [ MU.SubjectVerbSg "you" "drop"
       , MU.NWs 3 $ "royal blue vial" ]
       "You drop 3 royal blue vials."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "displace"
       , "you" ]
       "Haskell Alvin displaces you."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "drop"
       , MU.NWs 1 $ "royal blue vial" ]
       "Haskell Alvin drops a royal blue vial."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "gulp down"
       , MU.AW $ "royal blue vial" ]
       "Haskell Alvin gulps down a royal blue vial."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "feel better" ]
       "Haskell Alvin feels better."
  , tc [ MU.SubjectVerbSg "the royal blue vial" "turn out to be"
       , MU.NWs 1 $ "vial of healing (+5)" ]
       "The royal blue vial turns out to be a vial of healing (+5)."
  , tc [ MU.SubjectVerbSg "you" "gulp down"
       , MU.AW $ "magenta vial" ]
       "You gulp down a magenta vial."
  , tc [ MU.SubjectVerbSg "the magenta vial" "turn out to be"
       , MU.NWs 1 $ "vial of rose water" ]
       "The magenta vial turns out to be a vial of rose water."
  , tc [ MU.SubjectVerbSg "deranged household robot" "trie to hit"
         MU.:> ", but you block" ]
       "Deranged household robot tries to hit, but you block."
  , tc [ MU.SubjectVerbSg "deranged household robot" "hit"
       , "you" ]
       "Deranged household robot hits you."
  , tc [ MU.SubjectVerbSg "deranged household robot" "pick up"
       , MU.NWs 2 $ "sharpened pipe", "(3d1) (+1)" ]
       "Deranged household robot picks up 2 sharpened pipes (3d1) (+1)."
  , tc [ MU.SubjectVerbSg "deranged household robot" "hit"
       , MU.Text"you with", MU.NWs 1 $ "sharpened pipe (3d1) (+1)" ]
       "Deranged household robot hits you with a sharpened pipe (3d1) (+1)."
  , tc [ MU.SubjectVerbSg "you" "kick"
       , "deranged household robot" ]
       "You kick deranged household robot."
  , tc [ MU.SubjectVerbSg "deranged household robot" "die" ]
       "Deranged household robot dies."
  , tc [ MU.SubjectVerbSg "you" "find"
       , "a way downstairs" ]
       "You find a way downstairs."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "squash"
       , "you in a staircase accident" ]
       "Haskell Alvin squashes you in a staircase accident."
  , tc [ MU.SubjectVerbSg "you" "die" ]
       "You die."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "find"
       , "a way downstairs" ]
       "Haskell Alvin finds a way downstairs."
  , tc [ MU.SubjectVerbSg "Haskell Alvin" "hit"
       , "deranged household robot" ]
       "Haskell Alvin hits deranged household robot."
  , tc [ MU.SubjectVerbSg "deranged household robot" "hit"
       , "Haskell Alvin" ]
       "Deranged household robot hits Haskell Alvin."
  , tc [ MU.SubjectVerbSg "deranged household robot" "try to hit"
         MU.:> ", but Haskell Alvin blocks" ]
       "Deranged household robot tries to hit, but Haskell Alvin blocks."
  , tc [ MU.SubjectVerbSg "deformed monkey" "hit"
       , "deranged household robot" ]
       "Deformed monkey hits deranged household robot."
  , tc [ MU.SubjectVerbSg (MU.NWs 1 "flying billiard ball (1d1)") "hit"
       , "deranged household robot" ]
       "A flying billiard ball (1d1) hits deranged household robot."
  , tc [ MU.SubjectVerbSg "deranged household robot" "hiss in pain" ]
       "Deranged household robot hisses in pain."
  ]
