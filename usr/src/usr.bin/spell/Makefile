#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.9	(Berkeley)	%G%
#
# The word lists may be extended by adding special words.
# The set of files containing words to be added are defined
# by SPECIAL below.  By default we add commands and library
# function names defined in /usr/man/man[1238n].  The word
# lists also may be extended by adding local words.  The set
# of files containing words to be added are defined by LOCAL
# below.  Typically these lists reside in /usr/local/dict.

SPECIAL=	special.4bsd

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	spell.c spellin.c spellout.c
OBJS=	spell.o spellin.o spellout.o

all: spell hlist hlista hlistb hstop spellin spellout

spell: spell.c
	${CC} ${CFLAGS} spell.c -o $@

spellin: spellin.c
	${CC} ${CFLAGS} spellin.c -o $@

spellout: spellout.c
	${CC} ${CFLAGS} spellout.c -o $@

hlist: /usr/dict/words spellin
	spellin < words >hlist

hlista: american ${SPECIAL} ${LOCAL} hlist spellin
	(cat american ${SPECIAL} ${LOCAL}) | spellin hlist > hlista

hlistb: british ${SPECIAL} ${LOCAL} hlist spellin
	(cat british ${SPECIAL} ${LOCAL}) | spellin hlist > hlistb

hstop: stop spellin
	spellin < stop >hstop

clean: FRC
	rm -f ${OBJS} core spell spellin hlista hlistb hlist hstop spellout

depend: FRC
	mkdep -p ${CFLAGS} ${SRCS}

install: FRC
	-[ -d ${DESTDIR}/usr/dict/special ] || mkdir ${DESTDIR}/usr/dict/special
	install -s -o bin -g bin -m 755 spell ${DESTDIR}/usr/lib/spell
	install -s -o bin -g bin -m 755 spellin ${DESTDIR}/usr/bin/spellin
	install -s -o bin -g bin -m 755 spellout ${DESTDIR}/usr/bin/spellout
	install -c -o bin -g bin -m 755 spell.sh ${DESTDIR}/usr/bin/spell
	install -o bin -g bin -m 444 hlista ${DESTDIR}/usr/dict/hlista
	install -o bin -g bin -m 444 hlistb ${DESTDIR}/usr/dict/hlistb
	install -o bin -g bin -m 444 hstop ${DESTDIR}/usr/dict/hstop
	install -o bin -g bin -m 444 hlist ${DESTDIR}/usr/dict/hlist
	install -c -o bin -g bin -m 444 words ${DESTDIR}/usr/dict/words
	install -c -o bin -g bin -m 444 american ${DESTDIR}/usr/dict/american
	install -c -o bin -g bin -m 444 british ${DESTDIR}/usr/dict/british
	install -c -o bin -g bin -m 444 stop ${DESTDIR}/usr/dict/stop
	install -c -o bin -g bin -m 444 README ${DESTDIR}/usr/dict/README
	install -c -o bin -g bin -m 444 special.4bsd ${DESTDIR}/usr/dict/special/4bsd
	install -c -o bin -g bin -m 444 special.math ${DESTDIR}/usr/dict/special/math

lint: FRC
	lint ${CFLAGS} spell.c
	lint ${CFLAGS} spellin.c
	lint ${CFLAGS} spellout.c

tags: FRC
	ctags spell.c
	ctags -a spellin.c
	ctags -a spellout.c
	sort -o tags tags

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

spell: spell.c spell.h /usr/include/stdio.h /usr/include/ctype.h
spellin: spellin.c spell.h /usr/include/stdio.h /usr/include/ctype.h
spellout: spellout.c spell.h /usr/include/stdio.h /usr/include/ctype.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
