module Database.HDBC.MySQL.MP.Sql (
    insertNode,
    mpTable,
    insertNode'
) where


-- friends
import Database.HDBC.MySQL.MP.Join
import Database.HDBC.MySQL.MP.Path

-- std
import Database.HDBC

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BS


{-# INLINE braces #-}
braces s = "(" ++ s ++ ")"

{-# INLINE mark #-}
mark = "?"
marks = mark : marks

{-# INLINE prefixLike #-}
prefixLike :: String -> String
prefixLike s = s ++ "%"


joinPathWithId :: Maybe Path -> Int -> Path
joinPathWithId Nothing ident = mkPath [ident]
joinPathWithId (Just root) ident = joinPath root (mkPath [ident])

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
    run conn sql $ [toSql (showMaybePath root)] ++ values
    [[number]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    return (joinPathWithId root (fromSql number))

insertRootNode conn = insertNode conn Nothing
insertChildNode conn path = insertNode conn (Just path)

-- |Имя таблицы.
newtype TableName = TableName String

-- |Имена обязательных полей.
data StandartFields = StandartFields {
     pathField :: String, -- поле с путем
     keyField :: String,  -- ключевое поле (имя)
     nodeNameField :: String  -- поле для хранения уникального ключа
     }

defaultFields :: StandartFields
defaultFields = StandartFields "path" "caption" "node_name"

-- В общем случае узел дерева будет выглядеть так
data MPTable = MPTable {
    mpTableName :: String,
    mpStandartFields :: StandartFields,
    mpDataFields :: [String]
}

mpTable name dataFields = MPTable name defaultFields dataFields
    
mpTableFields :: MPTable -> [String]
mpTableFields table = let std = mpStandartFields table
                      in [pathField std, keyField std, nodeNameField std] ++ 
                         mpDataFields table


-- | Вычисляет уникальное имя узла на основе пути от корня и
-- уникального на уровне имени узла.
calcNodeName :: Maybe Path -> String -> String
calcNodeName path caption = 
    let pathString = case path of
                          Nothing -> ""
                          Just path -> show path
        key = pathString ++ "/" ++ caption
    in show (md5 (UTF8.fromString key))

insertNode' :: IConnection conn
            => conn
            -> MPTable
            -> Maybe Path
            -> String
            -> [(String, SqlValue)]
            -> IO Path
insertNode' conn table path caption values = do
    let fields = mpTableFields table
        dataFields = mpDataFields table
        sql = "INSERT INTO " ++ mpTableName table ++ 
              braces (join "," fields) ++
              " VALUES " ++ braces (join "," (take (length fields) marks))
        name = calcNodeName path caption
    vv <- valuesList dataFields values
    run conn sql $ [toSql (showMaybePath path), toSql caption, toSql name] ++ 
                   vv
    [[number]] <- quickQuery' conn "SELECT LAST_INSERT_ID()" []
    return (joinPathWithId path (fromSql number))
    where valuesList :: [String] -> [(String, SqlValue)] -> IO [SqlValue]
          valuesList [] _ = return []
          valuesList (f:fs) values = 
              case lookup f values of
                   Nothing -> fail $ "insertNode' value not found for " ++ f
                   Just v -> do tail <- valuesList fs values
                                return $ v : tail
    
--subtree :: IConnection conn
--           => conn
--           -> Path
--           -> IO [Path]
--subtree conn 