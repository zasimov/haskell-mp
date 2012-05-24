module Database.HDBC.MySQL.MP.Sql (
    insertNode
) where


-- friends
import Database.HDBC.MySQL.MP.Join
import Database.HDBC.MySQL.MP.Path

-- std
import Database.HDBC


{-# INLINE braces #-}
braces s = "(" ++ s ++ ")"


mark = "?"
marks = mark : marks


insertNode :: IConnection conn
           => conn
           -> Maybe Path
           -> String
           -> String
           -> [String]
           -> [SqlValue]
           -> IO Path
insertNode conn root table pathField dataFields values = do
    let sql = "INSERT INTO " ++ table ++ 
                braces (join "," ([pathField] ++ dataFields)) ++
              " VALUES " ++ braces (join "," (take (length dataFields + 1) marks))
    let pathString = case root of
                          Nothing -> ""
                          Just root -> show root
    run conn sql $ [toSql $ pathString] ++ values
    [[number]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    return $ case root of
                  Nothing -> mkPath [fromSql number]
                  Just root -> joinPath root (mkPath [fromSql number])
    