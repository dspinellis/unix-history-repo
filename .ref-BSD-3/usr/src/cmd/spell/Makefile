all:	spell hlist hlista hlistb hstop spellin spellout

install:	all
	install -s spell $(DESTDIR)/usr/lib
	install -s spellin ${DESTDIR}/usr/bin
	install -s spellout ${DESTDIR}/usr/bin
	install -c hlista $(DESTDIR)/usr/dict
	install -c hlistb $(DESTDIR)/usr/dict
	install -c hstop $(DESTDIR)/usr/dict
	install -c spell.sh $(DESTDIR)/usr/bin/spell

clean:	
	rm -f spell spellin hlista hlistb hlist hstop spellout

spell: spell.c
	cc -s -O spell.c -o spell
spellin: spellin.c
	cc -O -s spellin.c -o spellin
spellout: spellout.c
	cc -O -s spellout.c -o spellout

hlist: /usr/dict/words spellin
	spellin </usr/dict/words >hlist
hlista: american local hlist spellin
	(cat american local)|spellin hlist >hlista
hlistb: british local hlist spellin
	(cat british local)|spellin hlist >hlistb
hstop: stop spellin
	spellin <stop >hstop
