export PATH := $(PATH):/c/Program Files/Aseprite/:../../tools/cc65/bin/:../../go-nes/bin/

.PHONY: default pth

NESCFG = nes_mmc3.cfg
NAME = parallax

CAFLAGS = -g -t nes --color-messages
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map --color-messages

CHR = tiles_top.chr tiles_bot.chr sprites.chr
SOURCES = main.asm

default: bin/parallax.nes

bin/parallax.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ $<

bin/parallax.nes: bin/parallax.o $(NESCFG)
	ld65 $(LDFLAGS) -o $@ $<

tiles_top.chr: tiles_top.bmp
	chrutil -o $@ $^ --remove-duplicates --write-ids tiles_top.ids.asm

tiles_bot.chr: tiles_bot.bmp
	chrutil -o $@ $^ --remove-duplicates --write-ids tiles_bot.ids.asm

sprites.chr: scene-sprites.bmp
	chrutil -o $@ $^ --remove-duplicates --remove-empty
