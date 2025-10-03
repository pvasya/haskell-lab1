module Entities where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

-- Core entities representing each table

data User = User
  { userId   :: Int
  , username :: Text
  , email    :: Text
  , password :: Text
  , isAdmin  :: Bool
  } deriving (Show, Eq)

data Author = Author
  { authorId :: Int
  , authorName :: Text
  , authorFaculty :: Text
  } deriving (Show, Eq)

data Software = Software
  { softwareId     :: Int
  , title          :: Text
  , softwareAuthorId :: Int
  , version        :: Text
  , annotation     :: Text
  , softwareType   :: Text
  , usageTerms     :: Text
  , imageUrl       :: Maybe Text
  } deriving (Show, Eq)

data Distribution = Distribution
  { distSoftwareId :: Int
  , distPath       :: Text
  , uploadDate     :: UTCTime
  } deriving (Show, Eq)

data UsageStat = UsageStat
  { usSoftwareId  :: Int
  , usUserId      :: Int
  , usDate        :: UTCTime
  , actionsCount  :: Int
  } deriving (Show, Eq)

data Popularity = Popularity
  { popSoftwareId     :: Int
  , popUserId         :: Int
  , popularityScore   :: Int
  } deriving (Show, Eq)

-- Typeclasses and instances

class Printable a where
  header :: a -> String
  toRow  :: a -> String

instance Printable User where
  header _ = "ID | Username | Email | IsAdmin"
  toRow u = unwords
    [ show (userId u) ++ " |"
    , T.unpack (username u) ++ " |"
    , T.unpack (email u) ++ " |"
    , show (isAdmin u)
    ]

instance Printable Author where
  header _ = "ID | Name | Faculty"
  toRow a = unwords
    [ show (authorId a) ++ " |"
    , T.unpack (authorName a) ++ " |"
    , T.unpack (authorFaculty a)
    ]

instance Printable Software where
  header _ = "ID | Title | AuthorId | Version | Type"
  toRow s = unwords
    [ show (softwareId s) ++ " |"
    , T.unpack (title s) ++ " |"
    , show (softwareAuthorId s) ++ " |"
    , T.unpack (version s) ++ " |"
    , T.unpack (softwareType s)
    ]

instance Printable Distribution where
  header _ = "SoftwareId | Path | UploadDate"
  toRow d = unwords
    [ show (distSoftwareId d) ++ " |"
    , T.unpack (distPath d) ++ " |"
    , show (uploadDate d)
    ]

instance Printable UsageStat where
  header _ = "SoftwareId | UserId | Date | ActionsCount"
  toRow u = unwords
    [ show (usSoftwareId u) ++ " |"
    , show (usUserId u) ++ " |"
    , show (usDate u) ++ " |"
    , show (actionsCount u)
    ]

instance Printable Popularity where
  header _ = "SoftwareId | UserId | PopularityScore"
  toRow p = unwords
    [ show (popSoftwareId p) ++ " |"
    , show (popUserId p) ++ " |"
    , show (popularityScore p)
    ]

-- Helpers to convert raw query tuple results into entities

toUser :: (Int, Text, Text, Text, Int) -> User
toUser (i, un, em, pw, adm) =
  User { userId = i, username = un, email = em, password = pw, isAdmin = adm /= 0 }

toAuthor :: (Int, Text, Text) -> Author
toAuthor (i, n, f) = Author { authorId = i, authorName = n, authorFaculty = f }

toSoftware :: (Int, Text, Int, Text, Text, Text, Text, Maybe Text) -> Software
toSoftware (i, t, aid, ver, ann, typ, terms, img) =
  Software { softwareId = i, title = t, softwareAuthorId = aid, version = ver, annotation = ann, softwareType = typ, usageTerms = terms, imageUrl = img }

toDistribution :: (Int, Text, UTCTime) -> Distribution
toDistribution (sid, p, d) = Distribution { distSoftwareId = sid, distPath = p, uploadDate = d }

toUsageStat :: (Int, Int, UTCTime, Int) -> UsageStat
toUsageStat (sid, uid, dt, c) = UsageStat { usSoftwareId = sid, usUserId = uid, usDate = dt, actionsCount = c }

toPopularity :: (Int, Int, Int) -> Popularity
toPopularity (sid, uid, sc) = Popularity { popSoftwareId = sid, popUserId = uid, popularityScore = sc }