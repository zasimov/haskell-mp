module Database.HDBC.MySQL.MP.Join (join) where


import Data.List (intersperse)


-- FIXME: join здесь не подойдет
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)
