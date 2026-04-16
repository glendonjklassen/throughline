module Engine.Core.Axioms.Shared
  ( charIsSleeping
  ) where

import qualified Data.Map.Strict as Map

import           Engine.CRDT.ORSet
import           GameTypes

charIsSleeping :: CharId -> GameWorld -> Bool
charIsSleeping cid world =
  case Map.lookup cid (worldCharacters world) of
    Just c  -> orMember sleepingTag (charTags c)
    Nothing -> False
