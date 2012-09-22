module Model where

import Prelude
import Yesod
import Data.Aeson.Types as T
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Calendar
import Database.Persist.Quasi

import Model.Asana (Workspace)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (PomodoroGeneric backend) where
  toJSON p = T.object $ [ "startAt" .= pomodoroStartAt p
                        , "endAt" .= pomodoroEndAt p
                        , "taskId" .= pomodoroTaskId p
                        ]

instance ToJSON (BreakGeneric backend) where
  toJSON b = T.object $ [ "startAt" .= breakStartAt b
                        , "endAt" .= breakEndAt b
                        , "taskId" .= (Nothing :: Maybe Text)
                        ]

instance ToJSON (FinishedTaskGeneric backend) where
  toJSON t = T.object $ [ "finishOn" .= (showGregorian $ finishedTaskFinishOn t)
                        , "finishAt" .= finishedTaskFinishAt t
                        , "ident" .= finishedTaskIdent t
                        , "name" .= finishedTaskName t
                        , "status" .= finishedTaskStatus t
                        ]
