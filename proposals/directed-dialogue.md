# Proposal: Directed Dialogue

## Problem

`Say` currently models speech as a broadcast: `Say CharId String` — someone says something, but to nobody in particular. Real speech is directed. You talk *to* someone, or you mutter to the room. The distinction matters for:

- **Perception drift**: overheard speech should drift perception differently than speech directed at you
- **Relationship effects**: being spoken to directly is socially different from overhearing
- **Rendering**: the player should see who's being addressed
- **Future axioms**: NPCs reacting to being spoken to vs. overhearing conversation

## Current State

```haskell
-- Types.hs
Say CharId String           -- speaker + text, no listener
Think CharId String         -- internal, no audience (correct as-is)
Dialogue (NonEmpty (CharId, String))  -- multi-line, no listeners anywhere

-- DSL.hs
dialogueAction    :: ActionId -> String -> NonEmpty (CharId, String) -> Action 'Once
targetedDialogueAction :: ActionId -> String -> Entity -> NonEmpty (CharId, String) -> Action 'Once
```

`targetedDialogueAction` stores a target on the *action*, but each `Say` inside it has no audience. The action target and the speech target are conceptually different: an action targets the person you choose to interact with, but individual lines within that exchange could address different people (in a group conversation).

## Proposed Change

### 1. Add an audience to `Say`

```haskell
-- Before
Say CharId String

-- After  
Say CharId (Maybe CharId) String
--         ^ addressee (Nothing = said to the room)
```

`Maybe CharId` because speech genuinely can be undirected — muttering, announcements, thinking out loud. `Nothing` = "to the air."

### 2. Add audience to `Dialogue`

```haskell
-- Before
Dialogue (NonEmpty (CharId, String))

-- After
Dialogue (NonEmpty (CharId, Maybe CharId, String))
--                         ^ per-line addressee
```

Each line in a multi-line dialogue can address a different person. This supports group conversations naturally.

### 3. DSL helpers

New and updated helpers in `DSL.hs`:

```haskell
-- Directed speech (most common case)
say :: CharId -> CharId -> String -> EffectBody
say speaker listener text = Say speaker (Just listener) text

-- Undirected speech (announcements, muttering)
announce :: CharId -> String -> EffectBody
announce speaker text = Say speaker Nothing text

-- Think stays unchanged — thoughts have no audience
think :: CharId -> String -> EffectBody
think = Think
```

Update `dialogueChain` and `dialogue` to accept the new tuple shape. Provide a convenience wrapper for the common two-person conversation pattern:

```haskell
-- Two-person conversation: automatically sets each line's addressee to the other speaker
conversation :: CharId -> CharId -> NonEmpty (CharId, String) -> [Effect]
-- e.g. conversation you maya ((you, "Quiet night?") :| [(maya, "Always is...")])
-- Infers: you says "Quiet night?" TO maya, maya says "Always is..." TO you
```

This covers the 90% case (two people talking) without requiring the author to manually annotate every line.

### 4. Execution changes

In `Effects.hs`, `executeBody` for `Say` resolves the listener name alongside the speaker name:

```haskell
executeBody (Say speaker mListener text) = do
  w <- get
  let speakerName  = resolveName w speaker
      listenerName = fmap (resolveName w) mListener
  narrate (MsgSay speaker speakerName mListener listenerName text)
```

### 5. Narrative message update

```haskell
-- Before
MsgSay CharId String String  -- charId, resolvedName, text

-- After
MsgSay CharId String (Maybe CharId) (Maybe String) String
--                    ^ listener id   ^ listener name  ^ text
```

Same pattern for `MsgDialogue`.

### 6. Rendering

In `Display.hs`:

```
-- Directed speech
Maya (to Frank): "You look tired."

-- Undirected speech (to the air)
Maya: "What a night..."
```

The `(to Name)` annotation only appears when there's a listener. Undirected speech renders exactly as it does today — no breaking change to existing visual feel.

### 7. Scenario migration

Existing `Say cid text` constructors become `Say cid Nothing text` — currently undirected, which is the correct default for existing content. No scenario behavior changes. Scenarios can then be incrementally updated to use directed speech where it makes sense.

### 8. Think stays unchanged

`Think` has no audience by definition. Internal monologue is private. No changes needed.

## What this enables (but does NOT include)

These are future work that this proposal unblocks:

- **Perception drift from overheard speech**: the drift axiom could weight perception changes differently when you're the addressee vs. a bystander
- **NPC reaction axioms**: "when spoken to directly, react" vs. "when overhearing, react differently"
- **Social dynamics**: being addressed directly could affect trust/relationship differently
- **Narrative voice**: the engine could describe overheard speech differently ("You hear Maya say..." vs. direct address)

## Scope

- Modify `EffectBody` constructors: `Say`, `Dialogue`
- Modify `NarrativeMessage` constructors: `MsgSay`, `MsgDialogue`
- Update `executeBody` for both
- Update `Display.hs` rendering for both
- Add DSL helpers: `say`, `announce`, `conversation`
- Update existing DSL helpers: `dialogueChain`, `dialogueChainThen`, `dialogue`, `immediateDialogue`
- Update all call sites (scenarios + axioms) — mechanical, no behavior change
- Add tests for directed vs. undirected speech rendering
- Update existing dialogue tests for new constructor shape

## Not in scope

- Changing `Think` in any way
- Perception drift integration (separate axiom work)
- NPC reaction axioms (separate work)
- Changing `actionTarget` semantics (orthogonal concept, stays as-is)
