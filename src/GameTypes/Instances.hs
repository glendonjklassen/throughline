{-# OPTIONS_GHC -fno-hpc        #-}
{-# OPTIONS_GHC -Wno-orphans   #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Orphan Aeson (JSON) and Map key instances for game types.
module GameTypes.Instances where

import           GameTypes.Types

import           Data.Aeson      (ToJSON (..), FromJSON (..), ToJSONKey (..), FromJSONKey (..),
                                  object, (.=), (.!=), (.:), (.:?), withObject)
import           Data.Aeson.Types (FromJSONKeyFunction (..), Parser, toJSONKeyText)
import           Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(..))
import qualified Data.ByteString as BS
import           Data.Maybe      (catMaybes)
import qualified Data.Text       as T
import qualified Data.Text.Encoding as TE
import           Text.Read       (readMaybe)

-- CharId is used as a Map key; Show gives clean strings ("bradley", "Truth").
instance ToJSONKey CharId where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey CharId where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    pure $ case t of
      "Truth" -> Truth
      s       -> Named (T.unpack s)

-- StatType is used as a Map key; Show/Read round-trip via derived instances.
instance ToJSONKey StatType where
  toJSONKey = toJSONKeyText (T.pack . show)

instance FromJSONKey StatType where
  fromJSONKey = FromJSONKeyTextParser $ \t ->
    case readMaybe (T.unpack t) of
      Just st -> pure st
      Nothing -> fail ("Unknown StatType: " <> T.unpack t)

instance ToJSON   CharId
instance FromJSON CharId
instance ToJSON   Entity
instance FromJSON Entity
instance ToJSON   ActionId
instance FromJSON ActionId
instance ToJSON   WeatherDesc
instance FromJSON WeatherDesc
instance ToJSON   ClockTag
instance FromJSON ClockTag
instance ToJSON   FatigueLevel
instance FromJSON FatigueLevel
instance ToJSON   HungerLevel
instance FromJSON HungerLevel
instance ToJSON   SocialEnergyLevel
instance FromJSON SocialEnergyLevel
instance ToJSON   EngineTag
instance FromJSON EngineTag
instance ToJSON ScenarioTagValue where
  toJSON (MkScenarioTag s) = toJSON s

instance FromJSON ScenarioTagValue where
  parseJSON v = MkScenarioTag <$> parseJSON v

instance ToJSON   Tag
instance FromJSON Tag
instance ToJSON   CapacityStat
instance FromJSON CapacityStat
instance ToJSON   StatType
instance FromJSON StatType
instance ToJSON   Relationship
instance FromJSON Relationship
instance ToJSON   LiveEffect
instance FromJSON LiveEffect
instance ToJSON   Character
instance FromJSON Character
instance ToJSON   GameWorld
instance FromJSON GameWorld
instance ToJSON   Condition
instance FromJSON Condition
instance ToJSON   EffectBody
instance FromJSON EffectBody
instance ToJSON   Effect
instance FromJSON Effect
instance ToJSON (Action f)
instance FromJSON (Action f)

instance ToJSON AnyAction where
  toJSON (AnyAction a) = toJSON a

instance FromJSON AnyAction where
  parseJSON v = AnyAction <$> (parseJSON v :: Parser (Action 'Repeatable))
-- statDeltaPlayer / relationDeltaPlayer are always equal to entryPlayerId and
-- are redundant in every log entry. Omit from storage; LogEntry's FromJSON
-- patches them back from entryPlayerId on load.
--
-- The placeholder value is never observable outside deserialization:
-- LogEntry's FromJSON immediately overwrites it with the real entryPlayerId.

-- | Placeholder used during intermediate JSON parsing of deltas.
-- Immediately replaced by LogEntry's FromJSON; never reaches application code.
placeholderPlayer :: PlayerId
placeholderPlayer = PlayerId "<unpatched>"

instance ToJSON StatDelta where
  toJSON d = object
    [ "statDeltaChar" .= statDeltaChar d
    , "statDeltaStat" .= statDeltaStat d
    , "statDeltaOld"  .= statDeltaOld d
    , "statDeltaNew"  .= statDeltaNew d
    ]

instance FromJSON StatDelta where
  parseJSON = withObject "StatDelta" $ \o -> StatDelta
    <$> o .:  "statDeltaChar"
    <*> o .:  "statDeltaStat"
    <*> o .:  "statDeltaOld"
    <*> o .:  "statDeltaNew"
    <*> pure placeholderPlayer

instance ToJSON RelationDelta where
  toJSON d = object
    [ "relationDeltaFrom" .= relationDeltaFrom d
    , "relationDeltaTo"   .= relationDeltaTo d
    , "relationDeltaStat" .= relationDeltaStat d
    , "relationDeltaOld"  .= relationDeltaOld d
    , "relationDeltaNew"  .= relationDeltaNew d
    ]

instance FromJSON RelationDelta where
  parseJSON = withObject "RelationDelta" $ \o -> RelationDelta
    <$> o .: "relationDeltaFrom"
    <*> o .: "relationDeltaTo"
    <*> o .: "relationDeltaStat"
    <*> o .: "relationDeltaOld"
    <*> o .: "relationDeltaNew"
    <*> pure placeholderPlayer
instance ToJSON   Location
instance FromJSON Location
instance ToJSONKey Location where
  toJSONKey = toJSONKeyText (T.pack . locationName)
instance FromJSONKey Location where
  fromJSONKey = FromJSONKeyTextParser (pure . Location . T.unpack)
instance ToJSON   Region
instance FromJSON Region
instance ToJSON   LocationGraph
instance FromJSON LocationGraph
instance ToJSON   LocationDelta
instance FromJSON LocationDelta
instance ToJSON WorldDiff where
  toJSON d = object $ catMaybes
    [ nonEmpty "stats"            (diffStats d)
    , nonEmpty "relations"        (diffRelations d)
    , nonEmpty "tagsAdded"        (diffTagsAdded d)
    , nonEmpty "tagsRemoved"      (diffTagsRemoved d)
    , nonEmpty "worldTagsAdded"   (diffWorldTagsAdded d)
    , nonEmpty "worldTagsRemoved" (diffWorldTagsRemoved d)
    , nonEmpty "locations"        (diffLocations d)
    , nonEmpty "journal"          (diffJournal d)
    , if diffDayDelta d == 0 then Nothing else Just ("dayDelta" .= diffDayDelta d)
    ]
    where nonEmpty k xs = if null xs then Nothing else Just (k .= xs)

instance FromJSON WorldDiff where
  parseJSON = withObject "WorldDiff" $ \o -> WorldDiff
    <$> o .:? "stats"            .!= []
    <*> o .:? "relations"        .!= []
    <*> o .:? "tagsAdded"        .!= []
    <*> o .:? "tagsRemoved"      .!= []
    <*> o .:? "worldTagsAdded"   .!= []
    <*> o .:? "worldTagsRemoved" .!= []
    <*> o .:? "locations"        .!= []
    <*> o .:? "journal"          .!= []
    <*> o .:? "dayDelta"         .!= 0

instance ToJSON   PlayerId
instance FromJSON PlayerId

instance ToJSONKey PlayerId where
  toJSONKey = toJSONKeyText (\(PlayerId s) -> T.pack s)

instance FromJSONKey PlayerId where
  fromJSONKey = FromJSONKeyTextParser (pure . PlayerId . T.unpack)

instance ToJSON   LamportClock
instance FromJSON LamportClock

instance ToJSON LogEntry where
  toJSON e = object $ catMaybes
    [ Just ("id"       .= entryId e)
    , Just ("clock"    .= entryClock e)
    , Just ("player"   .= entryPlayerId e)
    , Just ("action"   .= entryActionId e)
    , Just ("diff"     .= entryDiff e)
    , (\sig -> "sig" .= TE.decodeUtf8 (convertToBase Base16 sig)) <$> entrySignature e
    , Just ("frontier" .= entryFrontier e)
    ]

instance FromJSON LogEntry where
  parseJSON = withObject "LogEntry" $ \o -> do
    eid      <- o .:  "id"
    clock    <- o .:  "clock"
    pid      <- o .:  "player"
    action   <- o .:  "action"
    diff     <- o .:  "diff"
    sig      <- o .:? "sig" >>= traverse decodeSig
    frontier <- o .:  "frontier"
    let patchedDiff = diff
          { diffStats     = map (\sd -> sd { statDeltaPlayer     = pid }) (diffStats diff)
          , diffRelations = map (\rd -> rd { relationDeltaPlayer = pid }) (diffRelations diff)
          }
    pure (LogEntry eid clock pid action patchedDiff sig frontier)
    where
      decodeSig t = case convertFromBase Base16 (TE.encodeUtf8 t) of
        Left  err -> fail ("Invalid signature encoding: " <> err)
        Right bs  -> pure (bs :: BS.ByteString)

instance ToJSON   Provenance
instance FromJSON Provenance
instance ToJSON a   => ToJSON   (MergeDelta a)
instance FromJSON a => FromJSON (MergeDelta a)
instance ToJSON   MergeDiff
instance FromJSON MergeDiff

instance ToJSON   AxiomId
instance FromJSON AxiomId
instance ToJSON   AxiomTrace
instance FromJSON AxiomTrace
instance ToJSON   Trigger
instance FromJSON Trigger
instance ToJSON   Target
instance FromJSON Target
instance ToJSON   AxiomRule
instance FromJSON AxiomRule
instance ToJSON   MergeTrigger
instance FromJSON MergeTrigger
instance ToJSON   MergeAxiomRule
instance FromJSON MergeAxiomRule

instance ToJSON   Narration
instance FromJSON Narration

instance ToJSON   Snapshot

-- Custom FromJSON: old snapshots may lack the three new fields,
-- so default them to empty lists for backward compatibility.
instance FromJSON Snapshot where
  parseJSON = withObject "Snapshot" $ \o -> Snapshot
    <$> o .:  "snapWorld"
    <*> o .:  "snapOffset"
    <*> o .:? "snapActions"    .!= []
    <*> o .:? "snapRules"      .!= []
    <*> o .:? "snapMergeRules" .!= []
