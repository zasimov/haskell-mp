module Main where


import Database.HDBC
import Database.HDBC.MySQL
import Database.HDBC.MySQL.MP


tasks = defaultMySQLConnectInfo {
                  mysqlHost = "localhost",
                  mysqlDatabase = "tasks",
                  mysqlUser = "root",
                  mysqlPassword = "mysql",
                  mysqlUnixSocket = "/opt/local/var/run/mysqld/mysqld.sock"
                  }

withDB :: IO Connection
withDB = connectMySQL tasks

-- Описываем таблицу - имя таблицы и дополнительные поля
taskTable = mpTable "task" ["tags", "priority"]

main = do
    db <- withDB
    insertNode' db taskTable (Just (read "13")) 
                "My task" [("tags", toSql "1,2,3"), 
                           ("priority", toSql "A")] >>= putStrLn . show
    disconnect db
    return ()