--------------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DiscordDB where

--------------------------------------------------------------------------------

import Database.Persist.Sql

import Discord.Types

--------------------------------------------------------------------------------

deriving instance PersistField ChannelId
deriving instance PersistFieldSql ChannelId

--------------------------------------------------------------------------------
