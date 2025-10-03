{-# LANGUAGE OverloadedStrings #-}
module CRUD where

import Data.Text
import Data.Time (UTCTime)

import Database.MySQL.Simple
    ( Connection, execute, query, query_, Only(..) )

import Entities
    ( Author,
      Distribution,
      Popularity,
      Software,
      UsageStat,
      User,
      toUser,
      toAuthor,
      toSoftware,
      toDistribution,
      toUsageStat,
      toPopularity )

-- Helpers
firstOrNothing :: [a] -> Maybe a
firstOrNothing (x:_) = Just x
firstOrNothing _     = Nothing

-- Authentication and user management
getUserByCredentials :: Connection -> Text -> Text -> IO (Maybe User)
getUserByCredentials conn un pw = do
  rows <- query conn
            "SELECT id, username, email, password, is_admin FROM users WHERE username = ? AND password = ? LIMIT 1"
            (un, pw) :: IO [(Int, Text, Text, Text, Int)]
  pure $ toUser <$> firstOrNothing rows

registerUser :: Connection -> Text -> Text -> Text -> IO ()
registerUser conn un em pw = do
  _ <- execute conn
        "INSERT INTO users (username, email, password, is_admin) VALUES (?, ?, ?, 0)"
        (un, em, pw)
  pure ()

createUserWithRole :: Connection -> Text -> Text -> Text -> Bool -> IO ()
createUserWithRole conn un em pw isAdm = do
  _ <- execute conn
        "INSERT INTO users (username, email, password, is_admin) VALUES (?, ?, ?, ?)"
        (un, em, pw, if isAdm then (1 :: Int) else 0)
  pure ()

updateUserAll :: Connection -> Int -> Text -> Text -> Text -> Bool -> IO ()
updateUserAll conn uid un em pw adminFlag = do
  _ <- execute conn
        "UPDATE users SET username = ?, email = ?, password = ?, is_admin = ? WHERE id = ?"
        (un, em, pw, if adminFlag then (1 :: Int) else 0, uid)
  pure ()

listUsers :: Connection -> IO [User]
listUsers conn = do
  rows <- query_ conn "SELECT id, username, email, password, is_admin FROM users" :: IO [(Int, Text, Text, Text, Int)]
  pure $ fmap toUser rows

deleteUser :: Connection -> Int -> IO ()
deleteUser conn uid = do
  _ <- execute conn "DELETE FROM users WHERE id = ?" (Only uid)
  pure ()

-- Authors
getAuthorIds :: Connection -> IO [Int]
getAuthorIds conn = do
  rows <- query_ conn "SELECT id FROM authors" :: IO [Only Int]
  pure $ fmap fromOnly rows

getAuthorById :: Connection -> Int -> IO (Maybe Author)
getAuthorById conn aid = do
  rows <- query conn "SELECT id, name, faculty FROM authors WHERE id = ?" (Only aid) :: IO [(Int, Text, Text)]
  pure $ toAuthor <$> firstOrNothing rows

listAuthors :: Connection -> IO [Author]
listAuthors conn = do
  rows <- query_ conn "SELECT id, name, faculty FROM authors" :: IO [(Int, Text, Text)]
  pure $ fmap toAuthor rows

createAuthor :: Connection -> Text -> Text -> IO ()
createAuthor conn name faculty = do
  _ <- execute conn "INSERT INTO authors (name, faculty) VALUES (?, ?)" (name, faculty)
  pure ()

updateAuthor :: Connection -> Int -> Text -> Text -> IO ()
updateAuthor conn aid name faculty = do
  _ <- execute conn "UPDATE authors SET name = ?, faculty = ? WHERE id = ?" (name, faculty, aid)
  pure ()

deleteAuthor :: Connection -> Int -> IO ()
deleteAuthor conn aid = do
  _ <- execute conn "DELETE FROM authors WHERE id = ?" (Only aid)
  pure ()

-- Software
getSoftwareIds :: Connection -> IO [Int]
getSoftwareIds conn = do
  rows <- query_ conn "SELECT id FROM software" :: IO [Only Int]
  pure $ fmap fromOnly rows

getSoftwareById :: Connection -> Int -> IO (Maybe Software)
getSoftwareById conn sid = do
  rows <- query conn
            "SELECT id, title, author_id, version, annotation, type, usage_terms, image_url FROM software WHERE id = ?"
            (Only sid) :: IO [(Int, Text, Int, Text, Text, Text, Text, Maybe Text)]
  pure $ toSoftware <$> firstOrNothing rows

listSoftware :: Connection -> IO [Software]
listSoftware conn = do
  rows <- query_ conn
            "SELECT id, title, author_id, version, annotation, type, usage_terms, image_url FROM software"
            :: IO [(Int, Text, Int, Text, Text, Text, Text, Maybe Text)]
  pure $ fmap toSoftware rows

createSoftware :: Connection -> Text -> Int -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
createSoftware conn ttl aid ver ann typ terms img = do
  _ <- execute conn
        "INSERT INTO software (title, author_id, version, annotation, type, usage_terms, image_url) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (ttl, aid, ver, ann, typ, terms, img)
  pure ()

updateSoftware :: Connection -> Int -> Text -> Text -> Text -> Text -> Maybe Text -> IO ()
updateSoftware conn sid ver ann typ terms img = do
  _ <- execute conn
        "UPDATE software SET version = ?, annotation = ?, type = ?, usage_terms = ?, image_url = ? WHERE id = ?"
        (ver, ann, typ, terms, img, sid)
  pure ()

deleteSoftware :: Connection -> Int -> IO ()
deleteSoftware conn sid = do
  _ <- execute conn "DELETE FROM software WHERE id = ?" (Only sid)
  pure ()

-- Distributions
getDistributionsBySoftwareId :: Connection -> Int -> IO [Distribution]
getDistributionsBySoftwareId conn sid = do
  rows <- query conn "SELECT software_id, path, upload_date FROM distributions WHERE software_id = ?" (Only sid) :: IO [(Int, Text, UTCTime)]
  pure $ fmap toDistribution rows

listDistributions :: Connection -> IO [Distribution]
listDistributions conn = do
  rows <- query_ conn "SELECT software_id, path, upload_date FROM distributions" :: IO [(Int, Text, UTCTime)]
  pure $ fmap toDistribution rows

createDistribution :: Connection -> Int -> Text -> IO ()
createDistribution conn sid path = do
  _ <- execute conn "INSERT INTO distributions (software_id, path, upload_date) VALUES (?, ?, NOW())" (sid, path)
  pure ()

updateDistributionPath :: Connection -> Int -> Text -> Text -> IO ()
updateDistributionPath conn sid oldPath newPath = do
  _ <- execute conn "UPDATE distributions SET path = ? WHERE software_id = ? AND path = ?" (newPath, sid, oldPath)
  pure ()

deleteDistribution :: Connection -> Int -> Text -> IO ()
deleteDistribution conn sid path = do
  _ <- execute conn "DELETE FROM distributions WHERE software_id = ? AND path = ?" (sid, path)
  pure ()

-- Usage stats
addUsageStat :: Connection -> Int -> Int -> IO ()
addUsageStat conn sid uid = do
  _ <- execute conn "INSERT INTO usage_stats (software_id, user_id, date, actions_count) VALUES (?, ?, NOW(), 1)" (sid, uid)
  pure ()

getUsageStatsBySoftware :: Connection -> Int -> IO [UsageStat]
getUsageStatsBySoftware conn sid = do
  rows <- query conn "SELECT software_id, user_id, date, actions_count FROM usage_stats WHERE software_id = ?" (Only sid) :: IO [(Int, Int, UTCTime, Int)]
  pure $ fmap toUsageStat rows

clearUsageStatsBySoftware :: Connection -> Int -> IO ()
clearUsageStatsBySoftware conn sid = do
  _ <- execute conn "DELETE FROM usage_stats WHERE software_id = ?" (Only sid)
  pure ()

-- Popularity
votePopularity :: Connection -> Int -> Int -> Int -> IO ()
votePopularity conn sid uid score = do
  _ <- execute conn
        "INSERT INTO popularity (software_id, user_id, popularity_score) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE popularity_score = VALUES(popularity_score)"
        (sid, uid, score)
  pure ()

getPopularityBySoftware :: Connection -> Int -> IO [Popularity]
getPopularityBySoftware conn sid = do
  rows <- query conn "SELECT software_id, user_id, popularity_score FROM popularity WHERE software_id = ?" (Only sid) :: IO [(Int, Int, Int)]
  pure $ fmap toPopularity rows

clearPopularityBySoftware :: Connection -> Int -> IO ()
clearPopularityBySoftware conn sid = do
  _ <- execute conn "DELETE FROM popularity WHERE software_id = ?" (Only sid)
  pure ()