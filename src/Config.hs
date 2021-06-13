--------------------------------------------------------------------------------

module Config where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.HashMap.Lazy as HM ( HashMap )
import Data.Text ( Text )

import Deriving.Aeson

import Discord.Types

import GHC.TypeLits ( Symbol )

--------------------------------------------------------------------------------

type JSONStripPrefix (str :: Symbol) =
    CustomJSON '[FieldLabelModifier (StripPrefix str, CamelToKebab)]

--------------------------------------------------------------------------------

data Config = MkConfig {
    cfgToken    :: Text,
    cfgChannels :: ChannelMap,
    cfgDB       :: DBConfig
} deriving Generic
  deriving FromJSON via JSONStripPrefix "cfg" Config

--------------------------------------------------------------------------------

-- Represents a map from exam codes to channel IDs
newtype ChannelMap = MkChannelMap { unChannelMap :: HashMap Text [ChannelId] }
  deriving Show

instance FromJSON ChannelMap where
    parseJSON = withObject "ChannelMap" (fmap MkChannelMap . mapM parseJSON)

--------------------------------------------------------------------------------

-- | The settings for the database connections
data DBConfig = MkDBConfig {
    dbHost     :: Text,
    dbPort     :: Int,
    dbDb       :: Text,
    dbUser     :: Text,
    dbPassword :: Text,
    dbPools    :: Int
} deriving Generic
  deriving FromJSON via JSONStripPrefix "db" DBConfig

--------------------------------------------------------------------------------
