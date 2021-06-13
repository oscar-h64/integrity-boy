--------------------------------------------------------------------------------

module ExamType where

--------------------------------------------------------------------------------

import Data.Text

import Database.Persist.Postgresql

import Text.Read

--------------------------------------------------------------------------------

data ExamType = TwentyFourHourWindow | FixedTime
    deriving (Read, Show, Eq)

instance PersistField ExamType where
    toPersistValue = PersistText . pack . show

    fromPersistValue (PersistText t) = case readMaybe (unpack t) of
        Just tt -> Right tt
        Nothing -> Left "Invalid type"
    fromPersistValue _ = Left "Invalid scope storage type"

instance PersistFieldSql ExamType where
    sqlType _ = SqlString 

--------------------------------------------------------------------------------
