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
--    , testMakePhraseSubjectVerb
    ]
  ]

tp :: [MU.Part] -> T.Text -> Test
tp arg expect =
  testCase ("testPhrase " ++ show arg)
  $ let obtain = MU.makePhrase MU.defIrrp arg
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
  , tp [MU.String "blue", MU.Text "dog"]   "blue dog"
  , tp [MU.String "blue", MU.Text ""]      "blue"
  , tp [MU.String "", MU.Text "dog"]       "dog"
  , tp [MU.String "", MU.Text ""]          ""
  , tp [MU.Text "blue", MU.String "dog"]   "blue dog"
  , tp [MU.Text "blue", MU.String ""]      "blue"
  , tp [MU.Text "", MU.String "dog"]       "dog"
  , tp [MU.Text "", MU.String ""]          ""
  , tp [MU.Text "blue", MU.Text "dog"]     "blue dog"
  , tp [MU.Text "blue", MU.Text ""]        "blue"
  , tp [MU.Text "", MU.Text "dog"]         "dog"
  , tp [MU.Text "", MU.Text ""]            ""
  , tp [MU.Compound (MU.Text "blue") (MU.Text "dog")] "blue dog"
  , tp [MU.Compound (MU.Text "blue") (MU.Text "")]    "blue"
  , tp [MU.Compound (MU.Text "") (MU.Text "dog")]     "dog"
  , tp [MU.Compound (MU.Text "") (MU.Text "")]        ""
  , tp [MU.String " "]                     " "
  , tp [MU.Text   " "]                     " "
  , tp [MU.String " blue ", MU.String " dog "]            " blue   dog "
  , tp [MU.Text " blue ", MU.Text " dog "]                " blue   dog "
  , tp [MU.Compound (MU.Text " blue ") (MU.Text " dog ")] " blue   dog "
  ]

testMakePhrasePlural :: Test
testMakePhrasePlural = testGroup "plural form Part constructors"
  [ tp [MU.Ws (MU.Text "dog")]        "dogs"
  , tp [MU.Ws (MU.Text "dogs")]       "dogs"
  , tp [MU.Ws (MU.Text "blue dog")]   "blue dogs"
  , tp [MU.Ws (MU.Text "blue dog  ")] "blue dog  "
  , tp [MU.Ws (MU.Text "blue dog.")]  "blue dog."
  , tp [MU.Ws (MU.Text "blue dog%")]  "blue dog%"
  , tp [MU.Ws (MU.Text "bitch")]      "bitches"
  , tp [MU.Ws (MU.Text "the fish")]   "the fishes"
  , tp [MU.Ws (MU.Text "miss")]       "misses"
  , tp [MU.Ws (MU.Text "buzz")]       "buzzes"
  , tp [MU.Ws (MU.Text "box")]        "boxes"
  , tp [MU.Ws (MU.Text "hajj")]       "hajjes"
  , tp [MU.Ws (MU.Text "goto")]       "gotoes"
  , tp [MU.Ws (MU.Text "igloo")]      "igloos"
  , tp [MU.Ws (MU.Text "buy")]        "buys"
  , tp [MU.Ws (MU.Text "try")]        "tries"
  , tp [MU.Ws (MU.Text "canto")]      "cantos"
  , tp [MU.Ws (MU.Text "homo")]      "homos"
  , tp [MU.Ws (MU.Text "photo")]      "photos"
  , tp [MU.Ws (MU.Text "nice zero")]  "nice zeros"
  , tp [MU.Ws (MU.Text "piano")]      "pianos"
  , tp [MU.Ws (MU.Text "portico")]    "porticos"
  , tp [MU.Ws (MU.Text "pro")]        "pros"
  , tp [MU.Ws (MU.Text "quarto")]     "quartos"
  , tp [MU.Ws (MU.Text "kimono")]     "kimonos"
  , tp [MU.Ws (MU.Text "calf")]       "calves"
  , tp [MU.Ws (MU.Text "leaf")]       "leaves"
  , tp [MU.Ws (MU.Text "knife")]      "knives"
  , tp [MU.Ws (MU.Text "life")]       "lives"
  , tp [MU.Ws (MU.Text "dwarf")]      "dwarfs"
  , tp [MU.Ws (MU.Text "hoof")]       "hooves"
  , tp [MU.Ws (MU.Text "elf")]        "elves"
--, tp [MU.Ws (MU.Text "staff")]      "staves"  -- depends on the meaning :(
  , tp [MU.Ws (MU.Text "child")]      "children"
  , tp [MU.Ws (MU.Text "foot")]       "feet"
  , tp [MU.Ws (MU.Text "goose")]      "geese"
  , tp [MU.Ws (MU.Text "louse")]      "lice"
  , tp [MU.Ws (MU.Text "man")]        "men"
  , tp [MU.Ws (MU.Text "mouse")]      "mice"
  , tp [MU.Ws (MU.Text "tooth")]      "teeth"
  , tp [MU.Ws (MU.Text "woman")]      "women"
  , tp [MU.Ws (MU.Text "buffalo")]    "buffalo"
  , tp [MU.Ws (MU.Text "deer")]       "deer"
  , tp [MU.Ws (MU.Text "the moose")]  "the moose"
  , tp [MU.Ws (MU.Text "sheep")]      "sheep"
  , tp [MU.Ws (MU.Text "bison")]      "bison"
  , tp [MU.Ws (MU.Text "salmon")]     "salmon"
  , tp [MU.Ws (MU.Text "pike")]       "pike"
  , tp [MU.Ws (MU.Text "trout")]      "trout"
  , tp [MU.Ws (MU.Text "swine")]      "swine"
  , tp [MU.Ws (MU.Text "aircraft")]   "aircraft"
  , tp [MU.Ws (MU.Text "watercraft")] "watercraft"
  , tp [MU.Ws (MU.Text "spacecraft")] "spacecraft"
  , tp [MU.Ws (MU.Text "hovercraft")] "hovercraft"
  , tp [MU.Ws (MU.Text "information")]     "information"
  , tp [MU.Ws (MU.String "dog blue")]      "dog blues"
  , tp [MU.Ws (MU.Ordinal 1)]              "firsts"
  , tp [MU.Ws (MU.Ws (MU.Text "do"))]      "doeses"
  , tp [MU.Ws (MU.NWs 1 (MU.Text "man"))]  "1 men"
  , tp [MU.Ws (MU.NthW 1 (MU.Text "man"))] "1st men"
  , tp [MU.Ws (MU.AW (MU.Text "elf"))]     "an elves"
  , tp [MU.Ws (MU.WWandW [MU.Text "dog", MU.Text "eagle", MU.Text "parrot"])]
                                           "dog, eagle and parrots"
  , tp [MU.Ws (MU.WWxW (MU.Text "and also")
                       [MU.Text "dog", MU.Text "eagle", MU.Text "parrot"])]
                                           "dog, eagle and also parrots"
  , tp [MU.Ws (MU.Wown (MU.Text "uncle"))]
                                           "uncle'ses"
  , tp [MU.Ws (MU.WownW (MU.Text "uncle") (MU.Text "dog"))]
                                           "uncle's dogs"
  , tp [MU.Ws (MU.Compound (MU.Text "uncle") (MU.Text "dog"))]
                                           "uncle dogs"
  , tp [MU.Ws (MU.SubjectVerb (MU.Text "I") (MU.Text "do"))]
                                           "I does"
  , tp [MU.Ws (MU.NotSubjectVerb (MU.Text "I") (MU.Text "do"))]
                                           "I don'ts"
  , tp [MU.Ws (MU.QSubjectVerb (MU.Text "woman") (MU.Text "do"))]
                                           "do women"
  ]

testMakePhraseNumber :: Test
testMakePhraseNumber = testGroup "number Part constructors"
  [ tp [MU.Cardinal 3]                  "three"
  , tp [MU.Cardinal 1111]               "1111"
  , tp [MU.Cardinal 131]                "131"
  , tp [MU.Cardinal 131, MU.Ordinal 2]  "131 second"
  , tp [MU.Cardinal (-3)]               "-3"
  , tp [MU.Cardinal 99999999999999992]  "99999999999999992"
  , tp [MU.Ordinal 3]                   "third"
  , tp [MU.Ordinal 1111]                "1111th"
  , tp [MU.Ordinal 131]                 "131st"
  , tp [MU.Ordinal 131, MU.Cardinal 2]  "131st two"
  , tp [MU.Ordinal (-3)]                "-3rd"
  , tp [MU.Ordinal 99999999999999992]   "99999999999999992nd"
  , tp [MU.NWs 1 (MU.Text "blue dog")]  "1 blue dog"
  , tp [MU.NWs 2 (MU.Text "blue elf")]  "2 blue elves"
  , tp [MU.NWs 2 (MU.Text " dog ")]     "2  dog "
  , tp [MU.NWs 3 (MU.Text "leaf")]      "3 leaves"
  , tp [MU.NWs 4 (MU.Text "sheep")]     "4 sheep"
  , tp [MU.NWs (-1) (MU.Text "dog")]    "-1 dogs"
  , tp [MU.NWs (-3) (MU.Text "dog")]    "-3 dogs"
  , tp [MU.NWs 12 (MU.Text "")]         "12"
  , tp [MU.NWs 5 (MU.Cardinal 1)]       "5 ones"
  , tp [MU.NWs 4 (MU.Ordinal 2)]        "4 seconds"
  , tp [MU.NthW 2 (MU.Text "blue dog")] "2nd blue dog"
  , tp [MU.NthW 2 (MU.Text " dog ")]    "2nd  dog "
  , tp [MU.NthW 3 (MU.Text "leaf")]     "3rd leaf"
  , tp [MU.NthW 4 (MU.Text "sheep")]    "4th sheep"
  , tp [MU.NthW (-3) (MU.Text "dog")]   "-3rd dog"
  , tp [MU.NthW 12 (MU.Text "")]        "12th"
  , tp [MU.NthW 51 (MU.Text "")]        "51st"
  , tp [MU.NthW 52 (MU.Text "")]        "52nd"
  , tp [MU.NthW 951 (MU.Text "")]       "951st"
  , tp [MU.NthW 952 (MU.Text "")]       "952nd"
  , tp [MU.NthW 112 (MU.Text "")]       "112th"
  , tp [MU.NthW 712 (MU.Text "")]       "712th"
  , tp [MU.NthW 5 (MU.Cardinal 1)]      "5th one"
  , tp [MU.NthW 4 (MU.Ordinal 2)]       "4th second"
  , tp [MU.NthW 4 (MU.NthW 7 (MU.Text "dog"))]    "4th 7th dog"
  , tp [MU.NthW 4 (MU.NWs 7 (MU.Text "dog"))]     "4th 7 dogs"
  , tp [MU.NWs 4 (MU.NWs 7 (MU.Text "dog"))]      "4 7 dogs"
  , tp [MU.NWs 4 (MU.NthW 7 (MU.Text "elf"))]     "4 7th elves"
  ]

testMakePhraseIndefinite :: Test
testMakePhraseIndefinite = testGroup "indefinite article"
  [ tp [MU.AW (MU.Text "blue")]                 "a blue"
  , tp [MU.AW (MU.Text "blue egg")]             "a blue egg"
  , tp [MU.AW (MU.Text "ABC")]                  "an ABC"
  , tp [MU.AW (MU.Text " ABC")]                 " ABC"
  , tp [MU.AW (MU.Text "ABC ")]                 "an ABC "
  , tp [MU.AW (MU.String "yell")]               "a yell"
  , tp [MU.AW (MU.Cardinal 3)]                  "a three"
  , tp [MU.AW (MU.Cardinal 8)]                  "an eight"
  , tp [MU.AW (MU.Cardinal 31)]                 "a 31"
  , tp [MU.AW (MU.Cardinal 83)]                 "an 83"
  , tp [MU.AW (MU.NWs 3 (MU.Text "dog"))]       "a 3 dogs"
  , tp [MU.AW (MU.Ordinal 3)]                   "a third"
  , tp [MU.AW (MU.Ordinal 8)]                   "an eighth"
  , tp [MU.AW (MU.Ordinal 31)]                  "a 31st"
  , tp [MU.AW (MU.Ordinal 83)]                  "an 83rd"
  , tp [MU.AW (MU.NthW 3 (MU.Text "dog"))]      "a 3rd dog"
  , tp [MU.AW (MU.AW (MU.Text "dog"))]          "an a dog"
  , tp [MU.AW (MU.WWandW [MU.Text "dog", MU.Text "eagle", MU.Text "parrot"])]
                                          "a dog, eagle and parrot"
  , tp [MU.AW (MU.WWxW (MU.Text "or otherwise")
                       [MU.Text "dog", MU.Text "eagle", MU.Text "parrot"])]
                                          "a dog, eagle or otherwise parrot"
  , tp [MU.AW (MU.Wown (MU.Text "uncle"))]
                                          "an uncle's"
  , tp [MU.AW (MU.WownW (MU.Text "uncle") (MU.Text "dog"))]
                                          "an uncle's dog"
  , tp [MU.AW (MU.Compound (MU.Text "uncle") (MU.Text "dog"))]
                                          "an uncle dog"
  , tp [MU.AW (MU.SubjectVerb (MU.Text "I") (MU.Text "do"))]
                                          "an I do"
  , tp [MU.AW (MU.NotSubjectVerb (MU.Text "I") (MU.Text "do"))]
                                          "an I don't"
  , tp [MU.AW (MU.QSubjectVerb (MU.Text "I") (MU.Text "do"))]
                                          "a do I"
  ]

testMakePhraseEnumeration :: Test
testMakePhraseEnumeration = testGroup "enumeration and collection"
  [ tp [MU.WWandW [MU.String "dog", MU.Text "eagle", MU.Cardinal 7]]
                                          "dog, eagle and seven"
  , tp [MU.WWxW (MU.Text "then")
                [MU.Ordinal 113, MU.AW (MU.String "eagle"), MU.Text "parrot"]]
                                          "113th, an eagle then parrot"
  , tp [MU.WWandW [ MU.String "I"
                  , MU.WWandW [MU.String "I", MU.Ordinal 31, MU.Cardinal 17]
                  , MU.WWandW [MU.Text "I", MU.AW (MU.Ordinal 18)]
                  ]]
                                          "I, I, 31st and 17 and I and an 18th"
  , tp [MU.WWxW (MU.Text " and perhaps%")
                [MU.Text " dog ", MU.Text "", MU.Text "%eagle."]]
                                          " dog   and perhaps% %eagle."
  ]

testMakePhrasePossesive :: Test
testMakePhrasePossesive = testGroup "the possesive form"
  [ tp [MU.Wown (MU.String "uncle")]                    "uncle's"
  , tp [MU.Wown (MU.String " uncle ")]                  " uncle "
  , tp [MU.Wown (MU.Text "")]                           ""
  , tp [MU.Wown (MU.Text " ")]                          " "
  , tp [MU.Wown (MU.Text "miss")]                       "miss'"
  , tp [MU.Wown (MU.Text "YQS")]                        "YQS'"
  , tp [MU.Wown (MU.Text "buzz")]                       "buzz's"
  , tp [MU.Wown (MU.Text "box")]                        "box's"
  , tp [MU.Wown (MU.Text "Who")]                        "Whose"
  , tp [MU.Wown (MU.Text "I")]                          "mine"
  , tp [MU.Wown (MU.Text "you")]                        "yours"
  , tp [MU.Wown (MU.Text "he")]                         "his"
  , tp [MU.Wown (MU.Text "She")]                        "Her"
  , tp [MU.Wown (MU.Text "it")]                         "its"
  , tp [MU.Wown (MU.Text "We")]                         "Ours"
  , tp [MU.Wown (MU.Text "they")]                       "theirs"
  , tp [MU.WownW (MU.String "uncle") (MU.String "dog")] "uncle's dog"
  , tp [MU.WownW (MU.Text " uncle ") (MU.Text "dog")]   " uncle  dog"
  , tp [MU.WownW (MU.Text "I") (MU.Text "")]            "my"
  , tp [MU.WownW (MU.Text "") (MU.Text "dog")]          "dog"
  , tp [MU.WownW (MU.Text "") (MU.Text "")]             ""
  , tp [MU.WownW (MU.Text " ") (MU.Text " ")]           "   "
  , tp [MU.WownW (MU.Text "miss") (MU.Text "dog")]      "miss' dog"
  , tp [MU.WownW (MU.Text "YQS") (MU.Cardinal 33)]      "YQS' 33"
  , tp [MU.WownW (MU.Text "buzz") (MU.Ordinal 21)]      "buzz's 21st"
  , tp [MU.WownW (MU.Text "box") (MU.Text "")]          "box's"
  , tp [MU.WownW (MU.Text "who") (MU.Text "dog")]       "whose dog"
  , tp [MU.WownW (MU.Text "I") (MU.Text "dog")]         "my dog"
  , tp [MU.WownW (MU.Text "you") (MU.Text "dog")]       "your dog"
  , tp [MU.WownW (MU.Text "He") (MU.Text "dog")]        "His dog"
  , tp [MU.WownW (MU.Text "she") (MU.Text "dog")]       "her dog"
  , tp [MU.WownW (MU.Text "It") (MU.Text "dog")]        "Its dog"
  , tp [MU.WownW (MU.Text "we") (MU.Text "dog")]        "our dog"
  , tp [MU.WownW (MU.Text "They") (MU.Text "dog")]      "Their dog"
  , tp [MU.Wown (MU.NWs 6 (MU.Text ""))]                "6's"
  , tp [MU.Wown (MU.NthW 1 (MU.Text ""))]               "1st's"
  , tp [MU.Wown (MU.Ws (MU.NWs 6 (MU.Text "")))]        "6es'"
  , tp [MU.Wown (MU.WWandW [MU.Text "I", MU.Text "you"])]
                                                        "I and yours"
  , tp [MU.Wown (MU.WWandW [MU.Text "you", MU.Text "I"])]
                                                        "you and mine"
  , tp [MU.WownW (MU.WWandW [MU.Text "you", MU.Text "I"]) (MU.Text "dog")]
                                                        "you and my dog"
  , tp [MU.Wown (MU.Wown (MU.Text "it"))]               "its'"
  , tp [MU.Wown (MU.Wown (MU.Wown (MU.Text "it")))]     "its's"
  , tp [MU.Wown (MU.QSubjectVerb (MU.Text "I") (MU.Text "do"))]
                                                        "do mine"
  , tp [MU.Wown (MU.Text " do   I")]                    " do   mine"
  , tp [MU.Wown (MU.Text " do   I ")]                   " do   I "
  ]
