{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Main (main) where

import           Control.Exception
import           Control.Monad
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Network.HTTP.Req          (MonadHttp (..))
import           Options.Applicative

import           Console.GitHubStats
import           Console.GitHubStats.Stats

data Options =
  Options { optOrganization :: String  -- ^ GitHub organization name
          }

options :: Parser Options
options = Options
  <$> strOption
    ( long "org"
   <> short 'o'
   <> showDefault
   <> value "stackbuilders"
   <> metavar "ORG"
   <> help "GitHub organization name" )

instance MonadHttp IO where
  handleHttpException = throwIO

main :: IO ()
main = do
  Options {..} <- execParser opts
  repos        <- mkHistogram <$> fetchRepos (T.pack optOrganization)

  forM_ repos T.putStrLn
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Computes repository stats from GitHub"
     <> header "GitHub Stats" )
