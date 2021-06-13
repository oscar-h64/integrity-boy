--------------------------------------------------------------------------------

module Lib (
    Env(..),
    lockUnlockThread,
) where

--------------------------------------------------------------------------------

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class ( liftIO )

import Data.Bits
import Data.HashMap.Lazy as HM ( HashMap, lookup )
import Data.List ( find )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Text ( Text )
import Data.Time.Clock

import Database.Persist
import Database.Persist.Sql

import Discord
import Discord.Requests
import Discord.Types

import Database
import ExamType

--------------------------------------------------------------------------------

data Env = MkEnv {
    envConnectionPool :: ConnectionPool,
    envConfig :: HashMap Text [ChannelId]
}

type BotM = ReaderT Env DiscordHandler

instance HasDBConn BotM where
    getPool = asks envConnectionPool

getChannelMap :: BotM (HashMap Text [ChannelId])
getChannelMap = asks envConfig

--------------------------------------------------------------------------------

lockUnlockThread :: BotM ()
lockUnlockThread = forever $ do
    -- Check for lock ops
    now <- liftIO getCurrentTime
    tasks <- runDB $ selectList [LockTriggerTime <=. now, LockDone ==. False] []
    mapM_ processLock tasks
    liftIO $ threadDelay 5_000_000

    -- Check for unlock ops
    now <- liftIO getCurrentTime
    tasks <- runDB $ selectList [UnlockTriggerTime <=. now, UnlockDone ==. False] []
    mapM_ processUnlock tasks
    liftIO $ threadDelay 5_000_000

--------------------------------------------------------------------------------

processLock :: Entity Lock -> BotM ()
processLock (Entity lockKey Lock{..}) = do
    -- Define lock message
    let lm = "Channel locked for exam. Good luck!"

    -- Get affected channels
    channels <- fromMaybe [] . HM.lookup lockExamCode <$> getChannelMap

    let lengthOfExam = case lockExamType of
            TwentyFourHourWindow -> 24*60*60
            FixedTime -> fromIntegral $ 60 * (((lockExamLength * 3) `div` 2) + 45)
    let unlockTime = addUTCTime lengthOfExam lockTriggerTime

    -- Lock channels
    unlockValues <- fmap catMaybes $ lift $ forM channels $ \c -> do
        let failure e = do
                lift $ print e
                restCall $ CreateMessage c "Error locking channel"
                pure Nothing

        eChannelData <- restCall (GetChannel c)
        flip (either failure) eChannelData $ \cd -> do
            let g = channelGuild cd
            let currentPerm = find ((== g) . overwriteId) (channelPermissions cd)
            let currentAllow = maybe 0 overwriteAllow currentPerm
            let currentDeny = maybe 0 overwriteDeny currentPerm
            let newPerm = ChannelPermissionsOpts{
                channelPermissionsOptsAllow = currentAllow .&. complement 0x800,
                channelPermissionsOptsDeny = currentDeny .|. 0x800,
                channelPermissionsOptsType = ChannelPermissionsOptsRole
            }
            res <- restCall $ EditChannelPermissions c g newPerm
            let unlockVal = Unlock{
                unlockTriggerTime = unlockTime,
                unlockChannel = c,
                unlockOriginalPermAllow = fromIntegral currentAllow,
                unlockOriginalPermDeny = fromIntegral currentDeny,
                unlockDone = False
            }
            either failure (const $ restCall (CreateMessage c lm) >> pure (Just unlockVal)) res

    -- Mark task as complete, and add unlock task
    runDB $ update lockKey [LockDone =. True]
    runDB $ insertMany_ unlockValues

--------------------------------------------------------------------------------

processUnlock :: Entity Unlock -> BotM ()
processUnlock (Entity unlockKey Unlock{..}) = do
    -- Create permission object
    let perm = ChannelPermissionsOpts{
        channelPermissionsOptsAllow = fromIntegral unlockOriginalPermAllow,
        channelPermissionsOptsDeny = fromIntegral unlockOriginalPermDeny,
        channelPermissionsOptsType = ChannelPermissionsOptsRole
    }

    -- Define unlock message
    let lm = "Channel Unlocked"

    -- Unlock channel
    let failure e = do
            lift $ print e
            void $ restCall $ CreateMessage unlockChannel "Error unlocking channel"

    -- Get guild and unlock channel
    eGuildId <- fmap channelGuild <$> lift (restCall $ GetChannel unlockChannel)
    lift $ case eGuildId of
        Left e -> failure e
        Right g -> restCall (EditChannelPermissions unlockChannel g perm)
               >>= either failure (const $ void $ restCall $ CreateMessage unlockChannel lm)

    -- Mark task as complete
    runDB $ update unlockKey [UnlockDone =. True]

--------------------------------------------------------------------------------
