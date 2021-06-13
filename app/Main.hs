--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Control.Monad.Logger ( runStderrLoggingT )
import Control.Monad.Reader

import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Text ( pack )
import Data.Text.Encoding ( encodeUtf8 )
import Data.Yaml

import Database.Persist.Postgresql

import Discord

import System.Environment

import Config
import Database
import Lib

--------------------------------------------------------------------------------

main :: IO ()
main = do
    -- Get config file path
    confPath <- fromMaybe "conf/config.yaml" . listToMaybe <$> getArgs

    -- Read config file
    MkConfig{..} <- decodeFileThrow confPath
    let MkDBConfig{..} = cfgDB

    -- components to join to form DB connection string
    let dbStr = [ "host=", dbHost
                , " port=", pack $ show dbPort
                , " user=", dbUser
                , " password=", dbPassword
                , " dbname=", dbDb
                ]

    -- open database connection
    sqlPool <- runStderrLoggingT
             $ createPostgresqlPool (encodeUtf8 $ mconcat dbStr) dbPools

    -- run automatic migrations
    runSqlPool (runMigration migrateAll) sqlPool

    -- Run bot
    let env = MkEnv sqlPool $ unChannelMap cfgChannels
    void $ runDiscord $ def{ discordToken = cfgToken
                           , discordOnStart = runReaderT lockUnlockThread env
                           }

--------------------------------------------------------------------------------
