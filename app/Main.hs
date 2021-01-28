module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

file = "./src/res/lvl/test.json"

main :: IO ()
-- main = readLevel file >>= (display FullScreen black).draw
main = readLevel file >>= (\x -> play FullScreen black 60 x draw control step)

control :: Event -> Level -> Level
control (EventKey (Char k) _ _ _) lvl = applyToPacman lvl (move (dir k) 0.5)
  where
    dir 'd' = R
    dir 'w' = U
    dir 'a' = L
    dir 's' = D
control _ lvl = lvl
