# Playing with a friend

Throughline has no server. Multiplayer works by each player running the game
independently and sharing session files. The engine's CRDT merge handles the
rest — your worlds converge without conflict.

This guide walks through a two-player deer hunt on two Windows machines.

## What you need

- Both players: [Stack](https://docs.haskellstack.org/en/stable/) installed
- A copy of the repo on each machine (clone or zip)
- A way to share files: USB stick, OneDrive, Dropbox, network share, git — anything

## Setup (each player, once)

### 1. Build

```
stack build
```

### 2. Set your name

First run creates your identity at:

```
%USERPROFILE%\.local\share\throughline\identity.key
```

Your display name lives next to it at `identity.label`. Edit that file to set
your name — it's plain text, one line:

```
echo Glen > %USERPROFILE%\.local\share\throughline\identity.label
```

This is cosmetic only. Your real identity is the Ed25519 keypair in
`identity.key`. Don't share that file.

### 3. Play

```
stack run
```

Pick "Deer Hunt" from the menu. Play as long as you like. Your session is saved
automatically to:

```
sessions/Deer Hunt/<your-player-id>/
  events.jsonl      # every action you took
  snapshot.json     # world state checkpoint
```

Your player ID is a long hex string derived from your keypair. You don't need
to memorize it.

## Sharing sessions

The merge protocol is file-based. To merge, you just need the other player's
session directory sitting next to yours.

### The simple version

After you've both played for a while:

1. **Player B** copies their folder:
   ```
   sessions/Deer Hunt/<player-b-id>/
   ```

2. **Player B** sends that folder to **Player A** (USB, OneDrive, zip, whatever)

3. **Player A** drops it into their own sessions directory:
   ```
   sessions/Deer Hunt/
     <player-a-id>/     ← yours
     <player-b-id>/     ← theirs (just added)
   ```

4. **Player A** starts the game:
   ```
   stack run
   ```

   The engine detects the foreign log and prompts:

   ```
   Merge 14 entries from Glen? (y/n)
   ```

   Accept, and the CRDT merge integrates their actions into your world. Their
   character now exists in your session. Your event log stays yours — the merge
   result goes into your snapshot.

5. Repeat in the other direction so Player B gets Player A's actions too.

### Live sync (shared folder)

You can point your session directory at any folder using the `--session-dir`
flag. By default sessions go to `sessions/` inside the project directory, but
if both players point at the same cloud-synced folder, merges happen live
while you play.

```
stack run -- --session-dir "C:\Users\You\OneDrive\throughline-sessions"
```

Your identity keys stay in your home directory
(`%USERPROFILE%\.local\share\throughline\`) no matter what. Only the session
logs move. This is by design — your identity is private, sessions are shared.

#### OneDrive

1. Create a folder in your OneDrive, something like `throughline-sessions`
2. Both players run the game pointing at that folder:
   ```
   stack run -- --session-dir "C:\Users\You\OneDrive\throughline-sessions"
   ```
3. Play whenever you want. OneDrive syncs the session files in the background.
4. Press **m** at the action prompt to merge. If your friend has new actions,
   they're folded into your world and you'll see it narrated in the story pane:

   ```
   > Glen's actions arrived. (3 merged)
   ```

   If there's nothing new, you'll see "No new actions to merge." and can
   pick a regular action.

At startup you're also prompted for any unmerged entries, same as before.

#### Google Drive

Same idea. Use Google Drive's local sync folder — wherever Drive for Desktop
maps to on your machine:

```
stack run -- --session-dir "C:\Users\You\Google Drive\throughline-sessions"
```

Create a `throughline-sessions` folder in Drive, share it with your friend,
and both point the game there.

#### Dropbox

Same pattern:

```
stack run -- --session-dir "C:\Users\You\Dropbox\throughline-sessions"
```

Any service that syncs a local folder works. The game doesn't care how the
files get there — it just watches the directory for new entries.

### Over git

You can also sync sessions through git if you prefer explicit control:

```
git add sessions/
git commit -m "session update"
git push
```

The other player pulls and relaunches. The `.gitignore` doesn't exclude
`sessions/` by default, so this works out of the box.

## What happens during a merge

- **Tags** (ORSet): add-wins. If you spotted the deer and they spooked it,
  both tags exist after merge.
- **Stats** (PNCounter): per-player deltas merge by high-water-mark. If you
  both lost hunger independently, the total reflects both losses.
- **Locations**: last-write-wins by Lamport clock. The deer ends up wherever
  the most recent action placed it.
- **Characters**: union. After merge, both hunters and the deer exist in both
  worlds.
- **Event log**: your log stays yours. The merge replays their divergent
  entries against your world state and writes the result to your snapshot.

Every log entry is signed with the author's Ed25519 key. The engine verifies
signatures during merge — tampered entries are rejected.

## Starting fresh

To wipe your session and start over:

```
stack run -- --new-session
```

Or just delete your folder under `sessions/Deer Hunt/`.

## Troubleshooting

**"Merge failed" warning**: Usually means a corrupted or truncated JSONL file.
Check that the file transfer completed. The game skips failed merges and
continues.

**Other player's character has no name**: They haven't set their
`identity.label` file. It defaults to "Player".

**Want to change your name**: Edit `identity.label` and relaunch. The name
updates in your session on next action.
