module Model where

import Prelude
import Yesod
import qualified Data.Aeson as A
import Data.Aeson.Types as T
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Calendar
import Database.Persist.Quasi

import Model.Asana (PersistWorkspace)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (FinishedTaskGeneric backend) where
  toJSON t = T.object $ [ "finishOn" .= (showGregorian $ finishedTaskFinishOn t)
                        , "finishAt" .= finishedTaskFinishAt t
                        , "ident" .= finishedTaskIdent t
                        , "name" .= finishedTaskName t
                        , "status" .= finishedTaskStatus t
                        ]
