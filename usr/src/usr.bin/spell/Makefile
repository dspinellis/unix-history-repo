#	@(#)Makefile	4.7	%G%

# The word lists may be extended by adding special words.
# The set of files containing words to be added are defined
# by SPECIAL below. By default we add commands and library
# function names defined in /usr/man/man[1238n]. The word
# lists also may be extended by adding local words. The set
# of files containing words to be added are defined by LOCAL
# below. Typically these lists reside in /usr/local/dict.

SPECIAL=special.4bsd
LOCAL=

CFLAGS= -O

all:	spell hlist hlista hlistb hstop spellin spellout

install:	all
	install -s spell $(DESTDIR)/usr/lib
	install -s spellin ${DESTDIR}/usr/bin
	install -s spellout ${DESTDIR}/usr/bin
	install -c spell.sh $(DESTDIR)/usr/bin/spell
	install -m 444 hlista $(DESTDIR)/usr/dict
	install -m 444 hlistb $(DESTDIR)/usr/dict
	install -m 444 hstop $(DESTDIR)/usr/dict
	install -m 444 hlist $(DESTDIR)/usr/dict
	install -m 444 -c words $(DESTDIR)/usr/dict
	install -m 444 -c american $(DESTDIR)/usr/dict
	install -m 444 -c british $(DESTDIR)/usr/dict
	install -m 444 -c stop $(DESTDIR)/usr/dict
	install -m 444 -c README $(DESTDIR)/usr/dict
	-mkdir $(DESTDIR)/usr/dict/special
	install -m 444 -c special.4bsd $(DESTDIR)/usr/dict/special/4bsd
	install -m 444 -c special.math $(DESTDIR)/usr/dict/special/math

clean:	
	rm -f *.o spell spellin hlista hlistb hlist hstop spellout

spell: spell.o
	cc spell.o -o spell
spellin: spellin.o
	cc spellin.o -o spellin
spellout: spellout.o
	cc spellout.o -o spellout

hlist: /usr/dict/words spellin
	spellin <words >hlist
hlista: american $(SPECIAL) $(LOCAL) hlist spellin
	(cat american $(SPECIAL) $(LOCAL))|spellin hlist >hlista
hlistb: british $(SPECIAL) $(LOCAL) hlist spellin
	(cat british $(SPECIAL) $(LOCAL))|spellin hlist >hlistb
hstop: stop spellin
	spellin <stop >hstop

depend:
