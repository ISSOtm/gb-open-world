# gb-open-world

Yes, again.

## Goal

This is my second attempt at an "open-world" Game Boy (Color-only) game engine.

What I mean by "open-world" is, the engine will be able to load graphics on the fly, without any "transitions" (no screen fades, no explicit design of a "transition area", no fixed screens, etc.).

This should enable creating games with much better graphical variety, bringing the experience much closer to more modern .

- It should be possible to create maps simply by **automatically converting PNG images**, without further editing.
  This is to make the engine easy to use.
- __Yes, the engine is GBC-only.__
  The GBC's extra VRAM bank and G/HDMA will be used a lot to make this possible.
  This should be possible on DMG on a lesser scale, but I'm thus not interested in that.
- **"12.4" fixed-point position support.**
  Fixed-point positions make smooth movement at any speed very easy.
  12.4 is convenient to work with on a technical level, and allows for 2048 by 2048 px maps, which should be plenty enough!
- **Pixel-based ("free") movement.**
  Because I find "grid-locked" movement clumsy, and it's harder to program, too.
  Plus, diagonals don't make much sense with it—MOTHER on the NES being a rare exception!

Additionally, I'm planning an extra twist music-wise :)

## History

I was originally planning to make such an engine back around 2018 (I think), with similar functionality.
However, I got involved in too many other projects at once, and when I came back, my own codebase was a mess I couldn't understand.

Since then, I evolved, tools improved, and I'm itching to get this done again.
Plus, there's just been a [new RGBDS pre-release](https://github.com/gbdev/rgbds/releases/tag/v0.5.0-rc1), so I'm gonna use this as an opportunity to test it!

## Technicalities

Better document some of the details that go behind all the streaming!

### VRAM layout

VRAM is divided in [two banks](https://gbdev.io/pandocs/#vram-banks-cgb-only), each containing 3 "blocks" (conceptually only) of 128 tiles each.
"Blocks" are only conceptual, but they're helpful to visualize the [access limitations](https://gbdev.io/pandocs/#vram-tile-data).

An important factor in deciding how to lay out VRAM is [HDMA](https://gbdev.io/pandocs/#lcd-vram-dma-transfers-cgb-only).
HDMA allows transferring data to VRAM more quickly (good!), but is dependent on the destination VRAM bank (and the source ROM bank, if copying from ROMX).
Thus, I decided that all VRAM that would be written to via HDMA would have to live in bank 0.
Bank 0 is accessed more often than bank 1 (I believe so, anyway), so this shouldn't be a problem.
It's just important to remember to ensure that HDMA is not running before switching VRAM banks!

Since the BG will be streamed in large chunks, it will use GDMA during VBlank.
VBlank lasts 1140 cycles ([4560 dots](https://gbdev.io/pandocs/#ff41-stat-lcd-status-r-w)), and copying 1 tile takes 8 cycles, so it's possible to copy 142.5 tiles per VBlank.
Reducing this to 128 tiles (the size of a single HDMA), this leaves ((142.5 - 128) * 8) = 116 cycles of potential overhead; assuming the transfer is performed within the VBlank handler, this would include interrupt dispatch, and setting up the transfer—that seems reasonable.
(@nitro2k01 suggested using a LY=LYC STAT interrupt to move some of the overhead to before VBlank; I might consider this if the need arises.)

After some thinking, I decided to have 64 "common" tiles shared by all tilesets, and reserve 64 BG tiles for UI (e.g. the textbox).

Another important feature I want, is the ability to dynamically load NPC cels.
I plan to use HDMA for this, since GDMA is already taken by BG loading.
Thus, they need to go into bank 0, as explained above.
Since they'll be loaded individually, there may be memory fragmentation; reserving more space should help combat this.
(TODO: another possibility is moving blocks towards the beginning of VRAM during idle time.)

Since the player is especially unpredictable, they could load new cels on each frame, and overload the loader; for this reason, the player's cels will instead be kept loaded at all times.
To still provide variety, 128 tiles are reserved in bank 1 for those.
There may still be some dynamic loading, but it should be more controlled.

This yields:

Block | Bank 0                     | Bank 1
------+----------------------------+-----------------------------------------
$8000 | NPC tiles                  | Player
$8800 | NPC tiles                  | UI (64 tiles), then common BG (64 tiles)
$9000 | 2 tilesets (64 tiles each) | 2 tilesets (64 tiles each)

### Palette streamer

Palettes are often the greatest enemy of an 8-bit artist... making large levels with so few colors (and color combinations, too!) tends to be an exercise in frustration...
Well, fear no more! For this wonderful component fixes that issue... somewhat.
"Hi-color" (aka rewriting palettes mid-frame) is not supported, so each individual screen must always conform to the same specifications as before, but nothing more!

Palettes are loaded dynamically as the camera pans.
This is accomplished by metatiles containing IDs that index into a "global" palette, instead of simply the hardware palette number.
Tiles leaving the screen decrease the palette's reference counter, incoming tiles increase it.

When a palette's reference counter reaches 0, its hardware slot becomes free.
Conversely, when a palette is first referenced, it gets loaded into a free hardware slot.
(If there are no free hardware slots, we have a problem.)
