{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Text (Text)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Data.List (intercalate)
import Control.Exception (try, SomeException)

import Entities
import DBConnection (getConn)
import CRUD
import Database.MySQL.Simple (Connection, close)

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  putStrLn "Вітаю в інформаційній системі програмного забезпечення факультету!"
  conn <- getConn
  mainMenu conn
  close conn

mainMenu :: Connection -> IO ()
mainMenu conn = do
  putStrLn "--------------------------------------"
  putStrLn "Виберіть опцію:"
  putStrLn "1. Авторизація"
  putStrLn "2. Реєстрація"
  putStrLn "3. Вихід"
  putStrLn "--------------------------------------"
  choice <- prompt "Ваш вибір: "
  case choice of
    "1" -> doLogin conn >> mainMenu conn
    "2" -> doRegister conn >> mainMenu conn
    "3" -> putStrLn "До побачення!"
    _    -> putStrLn "Невірна опція." >> mainMenu conn

-- IO helpers
prompt :: String -> IO String
prompt msg = do
  putStr msg
  hFlush stdout
  getLine

promptText :: String -> IO Text
promptText msg = T.pack <$> prompt msg

promptInt :: String -> IO (Maybe Int)
promptInt msg = do
  s <- prompt msg
  pure (readMaybe s :: Maybe Int)

printOne :: Printable a => a -> IO ()
printOne x = do
  putStrLn (header x)
  putStrLn (toRow x)

printMany :: Printable a => [a] -> IO ()
printMany xs = do
  if null xs then putStrLn "(порожньо)" else do
    putStrLn (header (head xs))
    mapM_ (putStrLn . toRow) xs

-- Auth flows
doLogin :: Connection -> IO ()
doLogin conn = do
  un <- promptText "Username: "
  pw <- promptText "Password: "
  eRes <- try (getUserByCredentials conn un pw) :: IO (Either SomeException (Maybe User))
  case eRes of
    Left _ -> putStrLn "Помилка запиту до БД. Запустіть init.sql, щоб оновити схему (поле password у users)."
    Right mUser -> case mUser of
      Nothing -> putStrLn "Невірні дані для входу."
      Just u  -> if isAdmin u then adminMenu conn u else userMenu conn u

doRegister :: Connection -> IO ()
doRegister conn = do
  un <- promptText "Новий username: "
  em <- promptText "Email: "
  pw <- promptText "Пароль: "
  eRes <- try (registerUser conn un em pw) :: IO (Either SomeException ())
  case eRes of
    Left _ -> putStrLn "Помилка запису у БД. Запустіть init.sql, щоб створити таблицю users."
    Right _ -> putStrLn "Реєстрація успішна. Можете авторизуватися."

-- User menu
userMenu :: Connection -> User -> IO ()
userMenu conn u = do
  putStrLn "--------------------------------------"
  putStrLn "Меню користувача"
  putStrLn "1. Змінити свої дані"
  putStrLn "2. Переглянути автора по Id"
  putStrLn "3. Переглянути програму по Id (з distributions)"
  putStrLn "4. Поставити оцінку програмі (1–10)"
  putStrLn "5. Вийти"
  putStrLn "--------------------------------------"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> do
      newUn <- promptText "Новий username: "
      newEm <- promptText "Новий email: "
      newPw <- promptText "Новий пароль: "
      updateUserAll conn (userId u) newUn newEm newPw False
      putStrLn "Дані оновлено."
      userMenu conn u { username = newUn, email = newEm, password = newPw }
    "2" -> do
      ids <- getAuthorIds conn
      putStrLn $ "Доступні Id авторів: " ++ (concatIds ids)
      mAid <- promptInt "Вкажіть Id автора: "
      case mAid of
        Nothing -> putStrLn "Невірне число." >> userMenu conn u
        Just aid -> do
          mA <- getAuthorById conn aid
          case mA of
            Nothing -> putStrLn "Автор не знайдений." >> userMenu conn u
            Just a  -> printOne a >> userMenu conn u
    "3" -> do
      ids <- getSoftwareIds conn
      putStrLn $ "Доступні Id програм: " ++ (concatIds ids)
      mSid <- promptInt "Вкажіть Id програми: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> userMenu conn u
        Just sid -> do
          mS <- getSoftwareById conn sid
          case mS of
            Nothing -> putStrLn "Програма не знайдена." >> userMenu conn u
            Just s  -> do
              printOne s
              dists <- getDistributionsBySoftwareId conn sid
              putStrLn "Дистрибутиви:" >> printMany dists
              addUsageStat conn sid (userId u)
              userMenu conn u
    "4" -> do
      mSid <- promptInt "Id програми: "
      mScore <- promptInt "Оцінка (1-10): "
      case (mSid, mScore) of
        (Just sid, Just sc) | sc >= 1 && sc <= 10 -> do
          votePopularity conn sid (userId u) sc
          addUsageStat conn sid (userId u)
          putStrLn "Голос враховано."
          userMenu conn u
        _ -> putStrLn "Невірні дані." >> userMenu conn u
    "5" -> putStrLn "Вихід у головне меню." >> pure ()
    _    -> putStrLn "Невірна опція." >> userMenu conn u

concatIds :: [Int] -> String
concatIds xs = if null xs then "(немає)" else intercalate "," (map show xs)

-- Admin menu
adminMenu :: Connection -> User -> IO ()
adminMenu conn u = do
  putStrLn "--------------------------------------"
  putStrLn "Меню адміністратора"
  putStrLn "1. Змінити свої дані"
  putStrLn "2. Статистика використання по програмі"
  putStrLn "3. Популярність програм"
  putStrLn "4. Таблиця users"
  putStrLn "5. Таблиця authors"
  putStrLn "6. Таблиця software"
  putStrLn "7. Таблиця distributions"
  putStrLn "8. Таблиця usage_stats"
  putStrLn "9. Таблиця popularity"
  putStrLn "10. Вихід"
  putStrLn "--------------------------------------"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> do
      newUn <- promptText "Новий username: "
      newEm <- promptText "Новий email: "
      newPw <- promptText "Новий пароль: "
      updateUserAll conn (userId u) newUn newEm newPw True
      putStrLn "Дані адміна оновлено."
      adminMenu conn u { username = newUn, email = newEm, password = newPw, isAdmin = True }
    "2" -> do
      mSid <- promptInt "Id програми: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> adminMenu conn u
        Just sid -> do
          stats <- getUsageStatsBySoftware conn sid
          printMany stats
          adminMenu conn u
    "3" -> do
      mSid <- promptInt "Id програми: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> adminMenu conn u
        Just sid -> do
          pops <- getPopularityBySoftware conn sid
          printMany pops
          adminMenu conn u
    "4" -> usersAdmin conn u
    "5" -> authorsAdmin conn u
    "6" -> softwareAdmin conn u
    "7" -> distributionsAdmin conn u
    "8" -> usageAdmin conn u
    "9" -> popularityAdmin conn u
    "10" -> putStrLn "Вихід у головне меню." >> pure ()
    _    -> putStrLn "Невірна опція." >> adminMenu conn u

-- Admin submenus
usersAdmin :: Connection -> User -> IO ()
usersAdmin conn u = do
  putStrLn "--- Users ---"
  putStrLn "1. Переглянути всіх"
  putStrLn "2. Створити"
  putStrLn "3. Оновити"
  putStrLn "4. Видалити"
  putStrLn "5. Назад"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> listUsers conn >>= printMany >> usersAdmin conn u
    "2" -> do
      un <- promptText "Username: "
      em <- promptText "Email: "
      pw <- promptText "Пароль: "
      isA <- prompt "Адмін? (y/n): "
      createUserWithRole conn un em pw (isA == "y")
      putStrLn "Користувача створено." >> usersAdmin conn u
    "3" -> do
      mId <- promptInt "Id користувача: "
      case mId of
        Nothing -> putStrLn "Невірне число." >> usersAdmin conn u
        Just uid -> do
          un <- promptText "Новий username: "
          em <- promptText "Новий email: "
          pw <- promptText "Новий пароль: "
          isA <- prompt "Адмін? (y/n): "
          updateUserAll conn uid un em pw (isA == "y")
          putStrLn "Оновлено." >> usersAdmin conn u
    "4" -> do
      mId <- promptInt "Id користувача: "
      maybe (putStrLn "Невірне число.") (\uid -> deleteUser conn uid >> putStrLn "Видалено.") mId
      usersAdmin conn u
    "5" -> adminMenu conn u
    _    -> usersAdmin conn u

authorsAdmin :: Connection -> User -> IO ()
authorsAdmin conn u = do
  putStrLn "--- Authors ---"
  putStrLn "1. Переглянути всіх"
  putStrLn "2. Створити"
  putStrLn "3. Оновити"
  putStrLn "4. Видалити"
  putStrLn "5. Назад"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> listAuthors conn >>= printMany >> authorsAdmin conn u
    "2" -> do
      n <- promptText "Ім'я: "
      f <- promptText "Факультет: "
      createAuthor conn n f
      putStrLn "Створено." >> authorsAdmin conn u
    "3" -> do
      mId <- promptInt "Id автора: "
      case mId of
        Nothing -> putStrLn "Невірне число." >> authorsAdmin conn u
        Just aid -> do
          n <- promptText "Нове ім'я: "
          f <- promptText "Новий факультет: "
          updateAuthor conn aid n f
          putStrLn "Оновлено." >> authorsAdmin conn u
    "4" -> do
      mId <- promptInt "Id автора: "
      maybe (putStrLn "Невірне число.") (\aid -> deleteAuthor conn aid >> putStrLn "Видалено.") mId
      authorsAdmin conn u
    "5" -> adminMenu conn u
    _    -> authorsAdmin conn u

softwareAdmin :: Connection -> User -> IO ()
softwareAdmin conn u = do
  putStrLn "--- Software ---"
  putStrLn "1. Переглянути всі"
  putStrLn "2. Створити"
  putStrLn "3. Оновити"
  putStrLn "4. Видалити"
  putStrLn "5. Назад"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> listSoftware conn >>= printMany >> softwareAdmin conn u
    "2" -> do
      ttl <- promptText "Назва: "
      mAid <- promptInt "Author Id: "
      case mAid of
        Nothing -> putStrLn "Невірне число." >> softwareAdmin conn u
        Just aid -> do
          ver <- promptText "Версія: "
          ann <- promptText "Анотація: "
          typ <- promptText "Тип: "
          terms <- promptText "Терміни використання: "
          img <- prompt "Image URL (можна пусто): "
          let mImg = if null img then Nothing else Just (T.pack img)
          createSoftware conn ttl aid ver ann typ terms mImg
          putStrLn "Створено." >> softwareAdmin conn u
    "3" -> do
      mSid <- promptInt "Id програми: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> softwareAdmin conn u
        Just sid -> do
          ver <- promptText "Нова версія: "
          ann <- promptText "Нова анотація: "
          typ <- promptText "Новий тип: "
          terms <- promptText "Нові терміни використання: "
          img <- prompt "Image URL (можна пусто): "
          let mImg = if null img then Nothing else Just (T.pack img)
          updateSoftware conn sid ver ann typ terms mImg
          putStrLn "Оновлено." >> softwareAdmin conn u
    "4" -> do
      mSid <- promptInt "Id програми: "
      maybe (putStrLn "Невірне число.") (\sid -> deleteSoftware conn sid >> putStrLn "Видалено.") mSid
      softwareAdmin conn u
    "5" -> adminMenu conn u
    _    -> softwareAdmin conn u

distributionsAdmin :: Connection -> User -> IO ()
distributionsAdmin conn u = do
  putStrLn "--- Distributions ---"
  putStrLn "1. Переглянути всі"
  putStrLn "2. Додати"
  putStrLn "3. Оновити шлях"
  putStrLn "4. Видалити"
  putStrLn "5. Назад"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> listDistributions conn >>= printMany >> distributionsAdmin conn u
    "2" -> do
      mSid <- promptInt "Software Id: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> distributionsAdmin conn u
        Just sid -> do
          p <- promptText "Шлях: "
          createDistribution conn sid p
          putStrLn "Створено." >> distributionsAdmin conn u
    "3" -> do
      mSid <- promptInt "Software Id: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> distributionsAdmin conn u
        Just sid -> do
          oldP <- promptText "Старий шлях: "
          newP <- promptText "Новий шлях: "
          updateDistributionPath conn sid oldP newP
          putStrLn "Оновлено." >> distributionsAdmin conn u
    "4" -> do
      mSid <- promptInt "Software Id: "
      case mSid of
        Nothing -> putStrLn "Невірне число." >> distributionsAdmin conn u
        Just sid -> do
          p <- promptText "Шлях: "
          deleteDistribution conn sid p
          putStrLn "Видалено." >> distributionsAdmin conn u
    "5" -> adminMenu conn u
    _    -> distributionsAdmin conn u

usageAdmin :: Connection -> User -> IO ()
usageAdmin conn u = do
  putStrLn "--- Usage stats ---"
  putStrLn "1. Переглянути по програмі"
  putStrLn "2. Очистити по програмі"
  putStrLn "3. Назад"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> do
      mSid <- promptInt "Id програми: "
      maybe (putStrLn "Невірне число.") (\sid -> getUsageStatsBySoftware conn sid >>= printMany) mSid
      usageAdmin conn u
    "2" -> do
      mSid <- promptInt "Id програми: "
      maybe (putStrLn "Невірне число.") (\sid -> clearUsageStatsBySoftware conn sid >> putStrLn "Очищено.") mSid
      usageAdmin conn u
    "3" -> adminMenu conn u
    _    -> usageAdmin conn u

popularityAdmin :: Connection -> User -> IO ()
popularityAdmin conn u = do
  putStrLn "--- Popularity ---"
  putStrLn "1. Переглянути по програмі"
  putStrLn "2. Очистити по програмі"
  putStrLn "3. Назад"
  ch <- prompt "Ваш вибір: "
  case ch of
    "1" -> do
      mSid <- promptInt "Id програми: "
      maybe (putStrLn "Невірне число.") (\sid -> getPopularityBySoftware conn sid >>= printMany) mSid
      popularityAdmin conn u
    "2" -> do
      mSid <- promptInt "Id програми: "
      maybe (putStrLn "Невірне число.") (\sid -> clearPopularityBySoftware conn sid >> putStrLn "Очищено.") mSid
      popularityAdmin conn u
    "3" -> adminMenu conn u
    _    -> popularityAdmin conn u