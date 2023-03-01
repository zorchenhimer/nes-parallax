#export PATH := $(PATH):/c/Program Files/Aseprite/:../../tools/cc65/bin/:../../go-nes/bin/

.PHONY: default pth

NESCFG = nes_mmc3.cfg
NAME = parallax

CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

CHRUTIL = go-nes/bin/chrutil

CHR = tiles_top.chr tiles_bot.chr sprites.chr
SOURCES = main.asm \
		  tiles_top.ids.asm \
		  tiles_bot.ids.asm

default: $(CHRUTIL) bin/ bin/parallax.nes

clean:
	-rm bin/*

bin/:
	mkdir bin

bin/parallax.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ $<

bin/parallax.nes: bin/parallax.o $(NESCFG)
	ld65 $(LDFLAGS) -o $@ $<

tiles_top.chr tiles_top.ids.asm: tiles_top.bmp
	$(CHRUTIL) -o tiles_top.chr $^ --remove-duplicates --write-ids tiles_top.ids.asm

tiles_bot.chr tiles_bot.ids.asm: tiles_bot.bmp
	$(CHRUTIL) -o tiles_bot.chr $^ --remove-duplicates --write-ids tiles_bot.ids.asm

sprites.chr: scene-sprites.bmp
	$(CHRUTIL) -o $@ $^ --remove-duplicates --remove-empty

$(CHRUTIL):
	$(MAKE) -C go-nes/ bin/chrutil
