{-# LANGUAGE DeriveGeneric #-}
module User where 

import Database.SQLite.Simple
import Data.Text (Text)
import Data.Csv
import GHC.Generics (Generic)

data User =
  User {
      userId   :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
  } deriving (Eq, Show, Generic)

instance FromRecord User

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir
              realName phone) =
    toRow (id_, username, shell, homeDir,
           realName, phone)
