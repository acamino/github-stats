{-# LANGUAGE OverloadedStrings #-}

module Console.GitHubStats.StatsSpec where

import           Test.Hspec

import           Console.GitHubStats.Stats
import           Console.GitHubStats.Types

spec :: Spec
spec =
  describe "histogram" $ do
    it "sorts languages in ascending order" $ do
      let repos =
            [ Repository { repoLanguage = Just "PureScript" }
            , Repository { repoLanguage = Just "Ruby" }
            , Repository { repoLanguage = Just "Haskell" }
            , Repository { repoLanguage = Just "Haskell" }
            , Repository { repoLanguage = Just "Ruby" }
            , Repository { repoLanguage = Just "Haskell" }
            ]
      histogram repos `shouldBe` [ "### Haskell 3"
                                 , "## Ruby 2"
                                 , "# PureScript 1"
                                 ]
    it "discards repositories without language" $ do
      let repos =
            [ Repository { repoLanguage = Nothing }
            , Repository { repoLanguage = Just "Haskell" }
            , Repository { repoLanguage = Just "Haskell" }
            ]
      histogram repos `shouldBe` [ "## Haskell 2" ]
