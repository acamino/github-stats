{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Console.GitHubStatsSpec where

import           Control.Exception
import           Data.Maybe
import           Network.HTTP.Req    (HttpException, MonadHttp (..))
import           Test.Hspec

import           Console.GitHubStats

instance MonadHttp IO where
  handleHttpException = throwIO

spec :: Spec
spec =
  describe "fetchRepos" $ do
    context "when the organization exists" $
      it "fetches a repository" $ do
        repository <- listToMaybe <$> fetchRepos "stackbuilders"
        repository `shouldSatisfy` isJust

    context "when the organization does not extist" $
      it "fetches a repository" $ do
        let action = fetchRepos "stackbuilders-bf39447aa673"
        action `shouldThrow` (const True :: Selector HttpException)
