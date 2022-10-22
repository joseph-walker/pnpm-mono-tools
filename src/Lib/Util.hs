module Lib.Util ( initMap ) where

initMap :: (a -> b) -> (a -> b) -> [a] -> [b]
initMap _ _ [] =
    []
initMap _ fnLst [a] =
    [fnLst a]
initMap fnFst fnLst [a, b] =
    [fnFst a, fnLst b]
initMap fnFst fnLst (a:bs) =
    fnFst a : initMap fnFst fnLst bs
