module User where 

import Control.Monad (mzero)
import Database.SQLite.Simple
import Data.Text (Text)
import Data.Csv

data User =
  User {
      userId   :: Maybe Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
  } deriving (Eq, Show)

instance FromRecord User where
  parseRecord v
    | length v == 5 = User Nothing
                        <$> v .! 0
                        <*> v .! 1
                        <*> v .! 2
                        <*> v .! 3
                        <*> v .! 4
    | otherwise     = mzero

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username_ shell_ homeDir_
              realName_  phone_) =
    toRow (id_, username_, shell_, homeDir_,
           realName_, phone_)
