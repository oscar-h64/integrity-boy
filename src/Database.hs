--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database (
    module ReExport,
    module Database
) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class      ( MonadIO, liftIO )

import Data.Text
import Data.Time.Clock

import Database.Persist
import Database.Persist            as ReExport ( Entity (..), Key (..) )
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH

import Discord.Types

import DiscordDB
import ExamType

--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Lock
    Id Int
    triggerTime UTCTime
    examCode Text
    examLength Int
    examType ExamType
    done Bool
Unlock
    Id Int
    triggerTime UTCTime
    channel ChannelId
    originalPermAllow Int
    originalPermDeny Int
    done Bool
|]

--------------------------------------------------------------------------------

class MonadIO m => HasDBConn m where
    getPool :: m ConnectionPool

runDB :: HasDBConn m => SqlPersistT IO a -> m a
runDB query = getPool >>= liftIO . runSqlPool query

--------------------------------------------------------------------------------
