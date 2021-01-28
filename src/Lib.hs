{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, genericToEncoding, toEncoding)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as I
import GHC.Generics
import Graphics.Gloss

avgSize = 16

-- | Class of objects that are vectorfields over a field b
class Vec a b | a -> b where
  (^+) :: a -> a -> a
  (^-) :: a -> a -> a
  (^*) :: b -> a -> a

-- | Class of objects a whose elements one can take the norm over the field b of
class Norm a b | a -> b where
  norm :: a -> b

-- | Class of objects whose elements can be drawn
class Drawable a where
  draw :: a -> Picture

-- | Class for objects whose elements can change over a duration of time / time-step
class Stepable a where
  step :: Float -> a -> a

-- | Class of objects whose elements can be moved in a Direction
class Movable a where
  move :: Direction -> Float -> a -> a

data Position = Position !Float !Float deriving (Eq, Show, Generic)

data Direction = R | U | L | D deriving (Eq, Show, Enum, Generic)

data Tile
  = Empty {position :: Position}
  | Wall {position :: Position}
  | Pallet {position :: Position}
  deriving (Eq, Generic)

data Entity
  = Pacman {pos :: Position, direction :: Direction, timer :: Float}
  | Ghost {pos :: Position, direction :: Direction, timer :: Float}
  deriving (Eq, Generic)

data Level = Level
  { levelName :: String,
    levelSize :: (Int, Int),
    levelTiles :: [Tile],
    levelEntities :: [Entity]
  }
  deriving (Eq, Generic)

instance Norm Position Float where
  norm (Position x y) = sqrt (x ** 2 + y ** 2)

instance Vec Position Float where
  Position x y ^+ Position v w = Position (x + v) (y + w)
  Position x y ^- Position v w = Position (x - v) (y - w)
  s ^* Position v w = Position (s * v) (s * w)

instance Show Tile where
  show (Empty _) = " "
  show (Wall _) = "▓"
  show (Pallet _) = "."

instance Enum Tile where
  fromEnum (Empty _) = 0
  fromEnum (Wall _) = 1
  fromEnum (Pallet _) = 2
  toEnum i
    | i == 0 = Empty (Position 0 0)
    | i == 1 = Wall (Position 0 0)
    | i == 2 = Pallet (Position 0 0)

instance Bounded Tile where
  minBound = Empty (Position 0 0)
  maxBound = Pallet (Position 0 0)

instance Norm Tile Float where
  norm (Wall p) = norm p
  norm (Pallet p) = norm p

instance Show Entity where
  show (Pacman _ U _) = "v"
  show (Pacman _ D _) = "^"
  show (Pacman _ L _) = ">"
  show (Pacman _ R _) = "<"
  show (Ghost _ _ _) = "👻"

instance Show Level where -- TODO
  show (Level n (x, y) t e) = n ++ "\n" ++ concat [showAt i j ++ if i == x then "\n" else "" | j <- [0 .. y], i <- [0 .. x]]
    where
      detectionRadius = 0.5
      tilesAt i j = filter (inRadius detectionRadius (Position (fromIntegral i) (fromIntegral j)) . position) t
      entityAt i j = filter (inRadius detectionRadius (Position (fromIntegral i) (fromIntegral j)) . pos) e
      showAt i j
        | entityAt i j /= [] = (show . head) (entityAt i j)
        | tilesAt i j /= [] = (show . head) (tilesAt i j)
        | otherwise = show (Empty (Position 0 0))

instance Drawable Entity where
  draw (Pacman (Position x y) dir time) = translate (avgSize * x) (avgSize * y) $ Color yellow $ arcSolid startangle endangle (avgSize / 2)
    where
      startangle = 90*fromIntegral (fromEnum dir) + mouthSize / 2
      endangle = 90*fromIntegral (fromEnum dir) +360 - mouthSize / 2
      mouthSize = 90 * abs (cos (2 * time))
  draw (Ghost (Position x y) dir time) = translate (avgSize * x) (avgSize * y) $ Color azure $ circleSolid (avgSize / 2)

instance Drawable Tile where
  draw (Empty _) = Blank
  draw (Wall (Position x y)) = translate (avgSize * x) (avgSize * y) $ Color (greyN 0.6) $ scale avgSize avgSize $ Polygon [(-0.5, -0.5), (0.5, -0.5), (0.5, 0.5), (-0.5, 0.5)]
  draw (Pallet (Position x y)) = translate (avgSize * x) (avgSize * y) $ Color (dark (dark yellow)) $ circleSolid (avgSize / 2 / 4)

instance Drawable Level where
  draw (Level name (sx, sy) tiles entities) = Pictures (map draw tiles) <> Pictures (map draw entities)

instance Stepable Entity where
  step epsilon (Pacman p d time) = Pacman p d (time + epsilon)
  step epsilon (Ghost p d time) = Ghost p d (time + epsilon)

instance Stepable Level where
  step epsilon (Level n s t e) = Level n s t (map (step epsilon) e)

instance Movable Entity where
  move dir1 dist (Pacman (Position x y) dir2 t) = Pacman newpos dir1 t
    where
      newpos
        | dir1 == R = Position (x+dist) y
        | dir1 == U = Position x (y+dist)
        | dir1 == L = Position (x-dist) y
        | dir1 == D = Position x (y-dist)
  move dir1 dist (Ghost (Position x y) dir2 t) = Ghost newpos dir1 t
    where
      newpos
        | dir1 == R = Position (x+dist) y
        | dir1 == U = Position x (y+dist)
        | dir1 == L = Position (x-dist) y
        | dir1 == D = Position x (y-dist)

instance ToJSON Position where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Position

instance ToJSON Direction where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Direction

instance ToJSON Tile where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Tile

instance ToJSON Entity where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Entity

instance ToJSON Level where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Level

inRadius :: Float -> Position -> Position -> Bool
inRadius r p q = norm (q ^- p) < r

addTile :: Level -> Tile -> Level
addTile (Level n s tiles e) t = Level n s (tiles ++ [t]) e

addTiles :: Level -> [Tile] -> Level
addTiles = foldl addTile

applyToEntities :: Level -> (Entity -> Entity) -> Level
applyToEntities (Level n s t e) f = Level n s t (map f e)

applyToPacman :: Level -> (Entity -> Entity) -> Level
applyToPacman lvl f = applyToEntities lvl (filterPacman f)
  where
    filterPacman :: (Entity -> Entity) -> Entity -> Entity
    filterPacman f (Pacman p d t) = f (Pacman p d t)
    filterPacman _ x = x

addBorder :: Level -> Level
addBorder l = addTiles l [Wall (Position (fromIntegral x) (fromIntegral y)) | x <- [0 .. sx], y <- [0 .. sy], x == 0 || x == sx || y == 0 || y == sy]
  where
    sx = (snd . levelSize) l
    sy = (fst . levelSize) l

saveLevel :: Level -> FilePath -> IO ()
saveLevel l = flip I.writeFile (encodeToLazyText l)

-- loadLevel :: FilePath -> IO Level
readLevel path = (\x -> fromJust (decode x :: Maybe Level)) <$> B.readFile path
