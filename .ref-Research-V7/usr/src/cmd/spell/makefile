all:	spell hlist hlista hlistb hstop spellin
	:

cp:	all
	cp spell /usr/lib
	cp hlista /usr/dict
	cp hlistb /usr/dict
	cp hstop /usr/dict
	rm spell hlista hlistb hstop hlist

cmp:	spell
	cmp spell /usr/lib/spell
	rm spell

spell: spell.c
	cc -i -s -O spell.c -o spell
spellin: spellin.c
	cc -i -O -s spellin.c -o spellin
spellout: spellout.c
	cc -i -O -s spellout.c -o spellout

hlist: /usr/dict/words spellin
	spellin </usr/dict/words >hlist
hlista: american local hlist spellin
	(cat american local)|spellin hlist >hlista
hlistb: british local hlist spellin
	(cat british local)|spellin hlist >hlistb
hstop: stop spellin
	spellin <stop >hstop
