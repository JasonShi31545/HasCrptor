
module AESFunctions where

import Data.Bits

shiftLeft :: [Char] -> Int -> [Char]
-- dat: data
-- off: offset
shiftLeft dat off = let front = foldl (\acc x -> acc ++ [x]) [] (dat !! i | i <- reverse [offset..3])
                    in front ++ dat

-- TODO: Finish implementing AESFunctions

-- shiftRight :: [Char] -> Int -> [Char]
-- shiftRight dat off = let length = length dat
