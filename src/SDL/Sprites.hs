-- | 8-bit style pixel sprites for terrain scatter.  Replaces the old
-- ASCII glyph scatter with small (5-8 px wide) colored pixel blobs
-- rendered through SDL primitives.  Everything here is seeded so a
-- single location's scatter stays identical across frames, but
-- different locations pick different sprite layouts so the world
-- doesn't tile.
module SDL.Sprites
  ( Sprite(..)
  , Pixel(..)
  , spritesForClass
  , drawSprite
  ) where

import           Foreign.C.Types (CInt)
import qualified SDL

import           SDL.FontContext (FontContext (..))
import           SDL.Palette     (Color (..))

-- | A single pixel in a sprite: offset from the sprite's origin plus
-- a color.  Pixel size is rendered as 'pixelScale' screen pixels on
-- each axis so sprites read as chunky 8-bit tiles on a modern display.
data Pixel = Pixel !Int !Int !Color

-- | A terrain sprite: a name (for debugging) and the pixels that
-- compose it.  Sprites are designed on a ~8×8 grid; a few are wider.
data Sprite = Sprite
  { spriteName   :: !String
  , spritePixels :: ![Pixel]
  } deriving (Show)

instance Show Pixel where
  show (Pixel x y _) = "Pixel " <> show x <> " " <> show y

-- | Sprite "pixel" size on screen.  3 gives sprites ~24px across,
-- chunky and readable against monospace text.
pixelScale :: CInt
pixelScale = 3

-- | Draw a sprite at the given pixel origin (top-left), modulated by
-- a uniform alpha (0-1).  Each @Pixel@ lands as a filled
-- @pixelScale × pixelScale@ square.
drawSprite :: FontContext -> (CInt, CInt) -> Double -> Sprite -> IO ()
drawSprite fc (ox, oy) alpha sprite = do
  let ren = fcRenderer fc
  SDL.rendererDrawBlendMode ren SDL.$= SDL.BlendAlphaBlend
  mapM_ (\(Pixel px py (Color r g b pa)) -> do
    let effA = min pa (round (fromIntegral pa * (alpha :: Double)))
        x    = ox + fromIntegral px * pixelScale
        y    = oy + fromIntegral py * pixelScale
        rect = SDL.Rectangle (SDL.P (SDL.V2 x y))
                             (SDL.V2 pixelScale pixelScale)
    SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b effA
    SDL.fillRect ren (Just rect)
    ) (spritePixels sprite)

-- ---------------------------------------------------------------------------
-- Sprite vocabulary per terrain class
-- ---------------------------------------------------------------------------

-- | Prairie-palette pixel colors, toned well down so the sprites
-- read as ambient texture rather than foreground.  Each is mixed
-- toward the background and rendered at a partial alpha so text
-- always dominates the eye.
--
-- Base palette values were the saturated earth-tones used in
-- earlier iterations; these are the same hues, darkened ~30% and
-- carrying an alpha of ~130 so the scatter sits quietly under
-- everything else on the panel.
colStraw, colStem, colDirt, colRock, colBark, colLeaf,
  colDarkLeaf, colWater, colMud, colGrass :: Color
colStraw    = Color 118 104  62 130   -- dry grass, dim
colStem     = Color  94  86  56 130   -- faded stalk
colDirt     = Color  84  70  52 130   -- furrow
colRock     = Color  78  74  66 130   -- weathered stone
colBark     = Color  70  56  44 130   -- trunk
colLeaf     = Color  62  74  54 130   -- sage leaf
colDarkLeaf = Color  48  58  42 130   -- deep canopy
colWater    = Color  62  80  86 130   -- creek water
colMud      = Color  58  48  42 130   -- dark damp earth
colGrass    = Color  80  86  58 130   -- cool grass

-- | Small grass tuft: 3 stalks leaning slightly.
grassTuft :: Sprite
grassTuft = Sprite "grassTuft"
  [ Pixel 1 2 colGrass
  , Pixel 1 1 colGrass
  , Pixel 1 0 colStraw
  , Pixel 2 2 colGrass
  , Pixel 2 1 colStraw
  , Pixel 3 2 colGrass
  , Pixel 3 1 colGrass
  ]

-- | Stubble tuft: dry straw remnants after harvest.
stubble :: Sprite
stubble = Sprite "stubble"
  [ Pixel 0 1 colStraw
  , Pixel 1 1 colStraw
  , Pixel 1 0 colStem
  , Pixel 2 1 colStraw
  , Pixel 3 1 colStraw
  , Pixel 3 0 colStem
  ]

-- | Loose dirt clump.
dirtClump :: Sprite
dirtClump = Sprite "dirtClump"
  [ Pixel 0 1 colDirt
  , Pixel 1 1 colDirt
  , Pixel 1 0 colMud
  , Pixel 2 1 colDirt
  ]

-- | Stalk: single standing dry plant.
stalk :: Sprite
stalk = Sprite "stalk"
  [ Pixel 0 0 colStem
  , Pixel 0 1 colStem
  , Pixel 0 2 colStraw
  ]

-- | Small rock.
rockSmall :: Sprite
rockSmall = Sprite "rockSmall"
  [ Pixel 0 1 colRock
  , Pixel 1 0 colRock
  , Pixel 1 1 colRock
  , Pixel 2 1 colRock
  ]

-- | Larger rock.
rockLarge :: Sprite
rockLarge = Sprite "rockLarge"
  [ Pixel 0 2 colRock
  , Pixel 1 1 colRock
  , Pixel 1 2 colRock
  , Pixel 2 0 colRock
  , Pixel 2 1 colRock
  , Pixel 2 2 colRock
  , Pixel 3 1 colRock
  , Pixel 3 2 colRock
  ]

-- | Bush leaves — dense clump of dark foliage.
bushClump :: Sprite
bushClump = Sprite "bushClump"
  [ Pixel 1 0 colLeaf
  , Pixel 2 0 colDarkLeaf
  , Pixel 0 1 colLeaf
  , Pixel 1 1 colDarkLeaf
  , Pixel 2 1 colLeaf
  , Pixel 3 1 colDarkLeaf
  , Pixel 1 2 colLeaf
  , Pixel 2 2 colLeaf
  ]

-- | Small poplar silhouette: trunk + leaf ball.
poplarSmall :: Sprite
poplarSmall = Sprite "poplarSmall"
  [ Pixel 1 0 colLeaf
  , Pixel 2 0 colLeaf
  , Pixel 1 1 colDarkLeaf
  , Pixel 2 1 colLeaf
  , Pixel 3 1 colDarkLeaf
  , Pixel 1 2 colLeaf
  , Pixel 2 2 colDarkLeaf
  , Pixel 2 3 colBark
  , Pixel 2 4 colBark
  ]

-- | Oak silhouette: broad dark canopy, short trunk.
oakSmall :: Sprite
oakSmall = Sprite "oakSmall"
  [ Pixel 1 0 colDarkLeaf
  , Pixel 2 0 colDarkLeaf
  , Pixel 3 0 colDarkLeaf
  , Pixel 0 1 colLeaf
  , Pixel 1 1 colDarkLeaf
  , Pixel 2 1 colDarkLeaf
  , Pixel 3 1 colDarkLeaf
  , Pixel 4 1 colLeaf
  , Pixel 1 2 colDarkLeaf
  , Pixel 2 2 colDarkLeaf
  , Pixel 3 2 colDarkLeaf
  , Pixel 2 3 colBark
  ]

-- | Creek water glint: three horizontal ripples.
waterGlint :: Sprite
waterGlint = Sprite "waterGlint"
  [ Pixel 0 0 colWater
  , Pixel 1 0 colWater
  , Pixel 2 0 colWater
  , Pixel 3 0 colWater
  , Pixel 1 1 colWater
  , Pixel 2 1 colWater
  ]

-- | Creek cattail — stalk with a dark head.
cattail :: Sprite
cattail = Sprite "cattail"
  [ Pixel 0 2 colStem
  , Pixel 0 1 colStem
  , Pixel 0 0 colBark
  ]

-- | Road gravel patch.
gravel :: Sprite
gravel = Sprite "gravel"
  [ Pixel 0 0 colRock
  , Pixel 2 0 colRock
  , Pixel 1 1 colRock
  , Pixel 3 1 colRock
  , Pixel 0 2 colRock
  , Pixel 2 2 colRock
  ]

-- | Fence post.
fencePost :: Sprite
fencePost = Sprite "fencePost"
  [ Pixel 0 0 colBark
  , Pixel 0 1 colBark
  , Pixel 0 2 colBark
  , Pixel 0 3 colBark
  ]

-- ---------------------------------------------------------------------------
-- Per-class sprite pools
-- ---------------------------------------------------------------------------

-- | Return the sprite vocabulary for a given class-like string.
-- Keyed by the same class names the generator emits (last word of the
-- region name: "Field", "Road", "Bush", "Ridge", "Creek").
spritesForClass :: String -> [Sprite]
spritesForClass name = case name of
  "Field"      -> [stubble, stalk, dirtClump, grassTuft]
  "Road"       -> [gravel, stubble, fencePost, dirtClump]
  "Bush"       -> [bushClump, poplarSmall, grassTuft, stubble]
  "Ridge"      -> [oakSmall, rockSmall, rockLarge, grassTuft]
  "Creek"      -> [waterGlint, cattail, rockSmall, bushClump]
  _            -> [grassTuft, dirtClump]
