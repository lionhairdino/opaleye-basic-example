{-# LANGUAGE Arrows #-}
module Main where

import Opaleye
import Database.PostgreSQL.Simple
import Data.Profunctor.Product (p3)
import Control.Arrow
import GHC.Int

userTable :: Table
  (Field SqlInt4, Field SqlText, Field SqlText)
  (Field SqlInt4, Field SqlText, Field SqlText)
userTable = table "users" (p3 (tableField "id",
                               tableField "name",
                               tableField "email"))

selectAllRows :: Connection -> IO [(Int, String, String)]
-- runQuery conn query 없어졌다.
-- queryTable
selectAllRows conn = runSelect conn $ selectTable userTable

insertRow :: Connection -> (Int, String, String) -> IO Int64 
insertRow conn (i,n,e) = do
  let q = Insert { iTable = userTable
                 , iRows = [(sqlInt4 i, sqlString n, sqlString e)]
                 , iReturning = rCount
                 , iOnConflict = Nothing
                 }
  runInsert conn q 

selectByEmail :: Connection -> String -> IO [(Int, String, String)]
selectByEmail conn email = runSelect conn selectQuery
  where
    selectQuery :: Select (Field SqlInt4, Field SqlText, Field SqlText)
    selectQuery = do
      row@(_, _, em) <- selectTable userTable
      where_ (em .== sqlString email)
      pure row

updateRow :: Connection -> (Int, String, String) -> IO Int64
updateRow conn row@(key, name, email) = do
  let q = Update 
            { uTable = userTable
            , uUpdateWith = updateEasy (\(i,n,e) -> (i, sqlString name, sqlString email))
            , uWhere = \(id_, _, _) -> id_ .== sqlInt4 key
            , uReturning = rCount
            }
  runUpdate conn q

main :: IO ()
main = do
  conn <- connect ConnectInfo{ connectHost="localhost"
                             , connectPort=5432
                             , connectDatabase="exampledb"
                             , connectPassword="coodb"
                             , connectUser="jacoo"
                             }
  allRows <- selectAllRows conn
  print allRows

  cn <- insertRow conn (5, "Saurabh", "saurabhnanda@gmail.com")
  print cn

  row <- selectByEmail conn "saurabhnanda@gmail.com"
  print row

  updateRow conn (5, "Don", "corleone@puzo.com")

  allRows <- selectAllRows conn
  print allRows

  return ()

