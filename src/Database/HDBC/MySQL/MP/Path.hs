module Database.HDBC.MySQL.MP.Path (
       Path(..),
       mkPath,   -- [Int] -> Path
       isRoot,   -- Path -> Bool
       getRoot,  -- Path -> Maybe Path
       joinPath  -- Path -> Path -> Path
) where

import Database.HDBC.MySQL.MP.Join (join)
import Data.List.Split (splitOn)


-- В списке ключей пути элементы должны быть немного перепутаны. 
-- Последний элемент должен находиться на первом месте, если
-- мы хотим ускорить операцию взятия корня.
-- Лучше, чтобы все было инвертировано!
data Path = Path [Int] Int


splitter = "."


joinReverse :: [a] -> [[a]] -> [a]
joinReverse delim l = join delim (reverse l)


pathToString :: Path -> String
pathToString (Path root number) =
             case root of
                 [] -> show number
                 xs -> (joinReverse splitter (map show xs)) ++ splitter ++ show number

instance Show Path where
         show = pathToString

readsPath :: ReadS Path
readsPath s = let path = map read (splitOn splitter s) 
              in [(mkPath path, "")]

instance Read Path where
    readsPrec _ = readsPath


mkPath :: [Int] -> Path
mkPath [x] = Path [] x
mkPath xs = Path (tail (reverse xs)) (head (reverse xs))

joinPath :: Path -> Path -> Path
joinPath (Path r1 n1) (Path r2 n2) = 
    Path (r2 ++ [n1] ++ r1) n2

isRoot (Path [] _) = True
isRoot _ = False

getRoot :: Path -> Maybe Path
getRoot path
    | isRoot path = Nothing
    |  otherwise = let (Path (x:xs) _) = path
                   in Just (Path xs x)
