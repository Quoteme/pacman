module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Set as S

type PressedKeys = S.Set Key
type World = (Level, PressedKeys)

file = "./src/res/lvl/test.json"
fps = 60

main :: IO ()
main = do
  lvl <- readLevel file
  let world = (lvl, S.empty)
  play FullScreen black fps world render input update

render :: World -> Picture
render (lvl,_) = draw lvl

input :: Event -> World -> World
input (EventKey k Down _ _) (lvl,p) = (lvl,S.insert k p)
input (EventKey k Up   _ _) (lvl,p) = (lvl,S.delete k p)
input _ lvl = lvl

update :: Float -> World -> World
update t (lvl,p) = control (step t lvl, p)

control :: World -> World
control (lvl,p)
  | S.member (Char 'd') p = (applyToPacman lvl (move R movementSpeed), p)
  | S.member (Char 'w') p = (applyToPacman lvl (move U movementSpeed), p)
  | S.member (Char 'a') p = (applyToPacman lvl (move L movementSpeed), p)
  | S.member (Char 's') p = (applyToPacman lvl (move D movementSpeed), p)
  | otherwise = (lvl,p)
  where
    movementSpeed = 3/fromIntegral fps
