module Aes where

-- TODO: Add encrypt and decrypt functions

import Data.Bits

process :: [Char] -> [Char]
process state = let spacesToAdd = ((length state) `div` 16 + 1) * 16 - length state
                in state ++ (take spacesToAdd $ repeat ' ')


unprocess :: [Char] -> [Char]
unprocess state = if last state == ' ' then unprocess (init state) else state


splitPT :: [Char] -> [[Char]]
splitPT [] = []
splitPT pt = let first16bytes = take 16 pt
             in first16bytes : splitPT (drop 16 pt)


