
SECTION "NPC gfx loader", ROMX

; Inits the NPC gfx heap
InitNPCGfxHeap:
	xor a
	ld hl, wNPCGfxHeap
	ld c, a ; ld c, 0
	jp MemsetSmall


; TODO: return a special flag to indicate that gfx are already loaded (when jumping to `.found`)
; @param d The NPC gfx ID to load
; @param e The amount of tiles to load, minus 1
; @return c The low byte of the ptr to the allocated block
;                  (i.e. the ID of the first free tile)
; @return! h HIGH(wNPCGfxHeap)
; @return! c LOW(Ptr to retained block, + 1)
; @destroy a l b
LoadNPCGfx:
	ld hl, wNPCGfxHeap
	; Look for either of two things:
	; - A slot still containing the gfx we want (to avoid re-loading them)
	; - A free slot large enough
	; Upon finding the former, stop immediately, and use that.
	; Upon finding the latter:
	;   1. keep a slot with exactly the right size;
	;   2. failing that, keep the slot with the largest size.
	;   This is done to try and combat fragmentation.
	lb bc, 0, $FF
.search
	ld a, [hli]
	cp d
	jr z, .found
	ld a, [hli] ; Read ref count
	and a ; Check if free
	ld a, [hld] ; Read ptr to next
	jr nz, .notFree
	; Block is free, keep it if it's larger
	sub l ; Compute block size **minus 1**
	; If exact match, branch immediately
	cp e
	jr z, .exactMatch
	; If blk size - 1 >= cur size (i.e. blk size > cur size), keep the new one
	cp b
	jr c, .smaller
	ld c, l
	ld b, a ; Update retained size
	inc b ; Compensate for blk size **minus 1**
.smaller
	add a, l ; Get back next ptr
.notFree
	ld l, a ; Move to next blk
	and a ; End if no more
	jr nz, .search

	; Assert that we did find a free block
	or b ; Check that the retained size isn't 0 (really 1)
	error z

	; Point to retained block
	dec c ; Point to gfx ID
	ld l, c

	; Mark block as containing our gfx
	ld [hl], d

	inc l ; Point back to ref count
.found
	; Mark the selected slot at referenced
	inc [hl]

	; Check to create an empty block
	; For that to be possible, there must be enough room to write the header
	; Condition is basically "&this->ptr + req_size < this->ptr"
	; (Since "&this->ptr + req_size" is the would-be address of the new block's last header byte)
	inc l ; Point to next ptr
	; Compute (putative) address of new block's last header byte
	ld a, l
	add a, e
	inc a ; We store "req_size - 1", so compensate for that
	; Check if it doesn't overlap the next block's header
	cp [hl]
	ret nc ; If not enough room for next block's header, count the extra space as part of this block

	; Link new free block into list
	sub 2 ; Compute address of new block's *first* header byte
	ld b, [hl] ; Store address of next block
	ld [hl], a ; Write ptr to new block, being the new next one

	; Write new free block's header
	xor a
	ld [hli], a ; No gfx are loaded here
	ld [hli], a ; Not referenced, either
	ld [hl], b ; Keep the chain intact
	ret

.exactMatch
	; Increase ref count (to 1)
	inc [hl] ; Just as fast as `ld [hl], 1`, but 1 byte smaller

	; Mark block as containing our gfx
	dec l ; Point to gfx ID
	ld [hl], d
	ld c, l
	ret


DefragNPCGfxHeap:

	;; TODO


SECTION "NPC gfx loader RAM", WRAM0,ALIGN[8]

; Each byte here corresponds to the respective tile in the VRAM NPC block ($8000-8FFF)
; Space usage is handled as a singly-linked list of indexes.
; Entries are 3 bytes:
; - NPC gfx ID (0 means nothing was ever loaded)
; - Ref count (0 means this slot is free)
; - Index of next entry (0 means this is the last entry)
; Note: two free blocks cannot be consecutive: freeing a block requires coalescing it with any free neighbors.
; Yes, entries are 3 bytes, but this is fine.
; Since 8x16 sprites are used, entries are always an even amount of bytes, so at least 2.
; It's impossible to condense entries to 2 bytes, so it's instead assumed that **all NPC entries are at least 4 tiles**
	WARN "NPC converter caveats below"
	;; Add check to converter that NPC entries are @least 4 tiles
	;; NPC IDs start at 1, since 0 is special
wNPCGfxHeap:
	ds 256
