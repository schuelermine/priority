{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tests (tests) where

import Data.Priority
import Distribution.TestSuite
import Test.QuickCheck
import Text.Read (readMaybe)

positiveIntType :: OptionType
positiveIntType =
  OptionNumber
    { optionNumberIsInt = True,
      optionNumberBounds = (Just "1", Nothing)
    }

qcTestOptions :: [OptionDescr]
qcTestOptions =
  [ OptionDescr
      { optionName = "maxSuccess",
        optionDescription = "Maximum number of successful tests before succeeding.",
        optionType = positiveIntType,
        optionDefault = Just "100"
      },
    OptionDescr
      { optionName = "maxDiscardRatio",
        optionDescription = "Maximum number of discarded tests per successful test before giving up",
        optionType = positiveIntType,
        optionDefault = Just "10"
      },
    OptionDescr
      { optionName = "maxSize",
        optionDescription = "Size to use for the biggest test cases",
        optionType = positiveIntType,
        optionDefault = Just "100"
      },
    OptionDescr
      { optionName = "chatty",
        optionDescription = "Whether QuickCheck should print anything",
        optionType = OptionBool,
        optionDefault = Just "True"
      },
    OptionDescr
      { optionName = "maxShrinks",
        optionDescription = "Maximum number of shrinks to before giving up",
        optionType =
          OptionNumber
            { optionNumberIsInt = True,
              optionNumberBounds = (Just "1", Nothing)
            },
        optionDefault = Just "True"
      }
  ]

optionParseError :: Either String b
optionParseError = Left "Option parse error"

setArg :: String -> String -> Args -> Either String Args
setArg "maxSuccess" s args = case readMaybe s of
  Nothing -> optionParseError
  Just m -> Right $ args {maxSuccess = m}
setArg "maxDiscardRatio" s args = case readMaybe s of
  Nothing -> optionParseError
  Just m -> Right $ args {maxDiscardRatio = m}
setArg "maxSize" s args = case readMaybe s of
  Nothing -> optionParseError
  Just m -> Right $ args {maxSize = m}
setArg "chatty" s args = case readMaybe s of
  Nothing -> optionParseError
  Just m -> Right $ args {chatty = m}
setArg "maxShrinks" s args = case readMaybe s of
  Nothing -> optionParseError
  Just m -> Right $ args {maxShrinks = m}
setArg _ _ _ = Left "Invalid option"

data QCTest t = QCTest
  { name :: String,
    tags :: [String],
    property :: t
  }

getTest :: forall t. Testable t => QCTest t -> Test
getTest QCTest {name, tags, property} =
  let runArgs args = do
        result <- quickCheckWithResult args property
        return $ Finished case result of
          r@(Success {}) -> Pass
          r@(GaveUp {}) -> Error $ "GaveUp: QuickCheck gave up" ++ "\n" ++ show r
          r@(Failure {}) -> Fail $ "Failure: A property failed" ++ "\n" ++ show r
          r@(NoExpectedFailure {}) -> Fail $ "NoExpectedFailure: A property that should have failed did not" ++ "\n" ++ show r
      withArgs args =
        TestInstance
          { name,
            tags,
            setOption = \opt val -> withArgs <$> setArg opt val args,
            run = runArgs args,
            options = qcTestOptions
          }
   in Test $ withArgs stdArgs

instance Arbitrary t => Arbitrary (Priority t) where
  arbitrary = sized \n ->
    frequency
      [ (1, Base <$> arbitrary),
        (n, Higher <$> arbitrary),
        (n, Lower <$> arbitrary)
      ]

getTests :: forall t. Testable t => [QCTest t] -> [Test]
getTests = (getTest <$>)

tests :: IO [Test]
tests =
  return
    [ Group
        { groupName = "pure-QuickCheck-Integer",
          concurrently = True,
          groupTests =
            getTests @(Priority Integer -> Bool)
              [ QCTest
                  { name = "Lower-x-lt-x",
                    tags = ["Ord", "Lower", "Integer"],
                    property = \x -> Lower x < x
                  },
                QCTest
                  { name = "Higher-x-gt-x",
                    tags = ["Ord", "Higher", "Integer"],
                    property = \x -> Higher x > x
                  },
                QCTest
                  { name = "Higher-x-gt-Lower-x",
                    tags = ["Ord", "Higher", "Lower", "Integer"],
                    property = \x -> Higher x > Lower x
                  }
              ]
              ++ [ Group
                     { groupName = "pure-QuickCheck-Integer-compareBase",
                       concurrently = True,
                       groupTests =
                         getTests @(Priority Integer -> Bool)
                           [ QCTest
                               { name = "compareBase-lower-x-to-x-is-LT",
                                 tags = ["compareBase", "Lower", "lower", "Integer"],
                                 property = \x -> compareBase (lower x) x == LT
                               },
                             QCTest
                               { name = "compareBase-Base-x-to-x-is-EQ",
                                 tags = ["compareBase", "Base", "Integer"],
                                 property = \x -> compareBase (Base x) x == EQ
                               },
                             QCTest
                               { name = "compareBase-higher-x-to-x-is-GT",
                                 tags = ["compareBase", "Higher", "higher", "Integer"],
                                 property = \x -> compareBase (higher x) x == GT
                               }
                           ]
                     }
                 ]
        }
    ]
