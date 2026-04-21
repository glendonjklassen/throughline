-- | Terrain properties keyed by 'TerrainClass'.  Noise and visibility
-- are how loud a location is to traverse and how exposed the hunter is
-- while standing there; both affect the deer's awareness model.
--
-- In the hand-authored map these were keyed per location with bespoke
-- cases for every oak thicket and gravel bar.  Under procedural
-- generation the class is the right abstraction — two procedurally
-- generated oak ridges should play the same way.
module Scenarios.DeerHunt.Terrain
  ( TerrainClass(..)
  , TerrainNoise(..)
  , TerrainVisibility(..)
  , classNoise
  , classVisibility
  , isRoadClass
  , isFieldClass
  , isCoverClass
  ) where

import Scenarios.DeerHunt.Generation (TerrainClass(..))

data TerrainNoise = Quiet | Moderate | Loud
  deriving (Show, Eq, Ord, Enum, Bounded)

data TerrainVisibility = Open | Partial | Dense
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Noise characteristic of a terrain class.  Roads and fields are
-- quiet (gravel or stubble underfoot, sound is expected and doesn't
-- travel).  Bush and ridges are loud (deadfall, leaves, dry branches).
-- Creeks are moderate — wet mud is quieter than dry leaves.
classNoise :: TerrainClass -> TerrainNoise
classNoise CRoad  = Quiet
classNoise CField = Quiet
classNoise CCreek = Moderate
classNoise CRidge = Loud
classNoise CBush  = Loud
classNoise CEmpty = Moderate

-- | Visibility characteristic of a terrain class.  Fields and roads
-- are open (nothing to block sight).  Ridges are open where they're
-- above the canopy.  Bush and creek vegetation block sight lines.
classVisibility :: TerrainClass -> TerrainVisibility
classVisibility CRoad  = Open
classVisibility CField = Open
classVisibility CRidge = Open
classVisibility CCreek = Partial
classVisibility CBush  = Dense
classVisibility CEmpty = Partial

isRoadClass :: TerrainClass -> Bool
isRoadClass CRoad = True
isRoadClass _     = False

isFieldClass :: TerrainClass -> Bool
isFieldClass CField = True
isFieldClass _      = False

-- | Cover classes: bush, ridge, creek.  Where deer feel safe.
isCoverClass :: TerrainClass -> Bool
isCoverClass c = c == CBush || c == CRidge || c == CCreek
