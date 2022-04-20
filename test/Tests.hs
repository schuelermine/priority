{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use /=" #-}
{-# HLINT ignore "Use ==" #-}
{-# HLINT ignore "Redundant compare" #-}
{-# HLINT ignore "Use >" #-}
{-# HLINT ignore "Use <" #-}

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
        { groupName = "pure-QuickCheck-Priority-Integer",
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
                     { groupName = "pure-QuickCheck-Eq-Laws-Priority-Integer",
                       concurrently = True,
                       groupTests =
                         [ eqReflexiveTest @(Priority Integer),
                           eqTransitiveTest @(Priority Integer),
                           eqIsNotNeqTest @(Priority Integer),
                           neqIsNotEqTest @(Priority Integer)
                         ]
                     },
                   Group
                     { groupName = "pure-QuickCheck-Eq-never-id",
                       concurrently = True,
                       groupTests =
                         [ functionIsNeverIdTest @(Priority Integer) Lower "Lower",
                           functionIsNeverIdTest @(Priority Integer) Higher "Higher"
                         ]
                     }
                 ]
        }
    ]

eqReflexiveTest :: forall a t. (Eq a, Testable t, t ~ (InfiniteList a -> Bool)) => Test
eqReflexiveTest =
  getTest
    QCTest
      { name = "Eq-eqReflexive",
        tags = ["Ord", "(==)", "reflexivity"],
        property = (\(getInfiniteList -> (x : _)) -> x == x) :: t
      }

eqTransitiveTest :: forall a t. (Eq a, Testable t, t ~ (InfiniteList a -> Property)) => Test
eqTransitiveTest =
  getTest
    QCTest
      { name = "Eq-eqTransitive",
        tags = ["Eq", "(==)", "transitivity"],
        property = (\(getInfiniteList -> (x : y : z : _)) -> x == y && y == z ==> x == z) :: t
      }

eqIsNotNeqTest :: forall a t. (Eq a, Testable t, t ~ (InfiniteList a -> Property)) => Test
eqIsNotNeqTest =
  getTest
    QCTest
      { name = "Eq-eqIsNotNeq",
        tags = ["Eq", "(/=)", "(==)"],
        property = (\(getInfiniteList -> (x : y : _)) -> x == y ==> not $ x /= y) :: t
      }

neqIsNotEqTest :: forall a t. (Eq a, Testable t, t ~ (InfiniteList a -> Property)) => Test
neqIsNotEqTest =
  getTest
    QCTest
      { name = "Eq-NeqIsNotEq",
        tags = ["Eq", "(/=)", "(==)"],
        property = (\(getInfiniteList -> (x : y : _)) -> x /= y ==> not $ x == y) :: t
      }

compareLTIsLt :: forall a t. (Ord a, Testable t, t ~ (InfiniteList a -> Property), Ord a) => Test
compareLTIsLt =
  getTest
    QCTest
      { name = "Eq-compareEQisEq",
        tags = ["Eq", "compare", "LT", "(<)"],
        property = (\(getInfiniteList -> (x : y : _)) -> compare x y == LT ==> x < y) :: t
      }

ltIsCompareLT :: forall a t. (Ord a, Testable t, t ~ (InfiniteList a -> Property), Ord a) => Test
ltIsCompareLT =
  getTest
    QCTest
      { name = "Eq-compareEQisEq",
        tags = ["Eq", "compare", "LT", "(<)"],
        property = (\(getInfiniteList -> (x : y : _)) -> x < y ==> compare x y == LT) :: t
      }

compareEQIsEq :: forall a t. (Ord a, Testable t, t ~ (InfiniteList a -> Property), Ord a) => Test
compareEQIsEq =
  getTest
    QCTest
      { name = "Eq-compareEQisEq",
        tags = ["Eq", "compare", "EQ", "(==)"],
        property = (\(getInfiniteList -> (x : y : _)) -> compare x y == EQ ==> x == y) :: t
      }

eqIsCompareEQ :: forall a t. (Ord a, Testable t, t ~ (InfiniteList a -> Property), Ord a) => Test
eqIsCompareEQ =
  getTest
    QCTest
      { name = "Eq-compareEQisEq",
        tags = ["Eq", "compare", "EQ", "(==)"],
        property = (\(getInfiniteList -> (x : y : _)) -> x == y ==> compare x y == EQ) :: t
      }

compareGTIsGt :: forall a t. (Ord a, Testable t, t ~ (InfiniteList a -> Property), Ord a) => Test
compareGTIsGt =
  getTest
    QCTest
      { name = "Eq-compareEQisEq",
        tags = ["Eq", "compare", "GT", "(>)"],
        property = (\(getInfiniteList -> (x : y : _)) -> compare x y == GT ==> x > y) :: t
      }

gtIsCompareGT :: forall a t. (Ord a, Testable t, t ~ (InfiniteList a -> Property), Ord a) => Test
gtIsCompareGT =
  getTest
    QCTest
      { name = "Eq-compareEQisEq",
        tags = ["Eq", "compare", "GT", "(>)"],
        property = (\(getInfiniteList -> (x : y : _)) -> x > y ==> compare x y == GT) :: t
      }

functionIsNeverIdTest :: forall a t. (Eq a, Testable t, t ~ (InfiniteList a -> Bool)) => (a -> a) -> String -> Test
functionIsNeverIdTest f n =
  getTest
    QCTest
      { name = "Eq-functionIsNeverId-" ++ n,
        tags = ["Eq", "id", "(/=)", n],
        property = \(getInfiniteList -> (x : y : _)) -> f x /= x
      }
