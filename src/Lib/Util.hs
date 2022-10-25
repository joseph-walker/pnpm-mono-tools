module Lib.Util ( initMap, maybeFilter ) where

initMap :: (a -> b) -> (a -> b) -> [a] -> [b]
initMap _ _ [] =
    []
initMap _ fnLst [a] =
    [fnLst a]
initMap fnFst fnLst [a, b] =
    [fnFst a, fnLst b]
initMap fnFst fnLst (a:bs) =
    fnFst a : initMap fnFst fnLst bs

maybeFilter :: Eq a => Maybe a -> [(a, b)] -> [(a, b)]
maybeFilter =
    maybe id (\ f -> filter ((== f) . fst))
