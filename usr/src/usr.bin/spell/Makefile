#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.10	(Berkeley)	%G%
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
MAN=	spell.0

all: spell hlist hlista hlistb hstop spellin spellout

spell spellin spellout: ${LIBC}
	${CC} ${CFLAGS} $@.c -o $@

hlist: /usr/dict/words spellin
	spellin < words >hlist

hlista: american ${SPECIAL} ${LOCAL} hlist spellin
	(cat american ${SPECIAL} ${LOCAL}) | spellin hlist > hlista

hlistb: british ${SPECIAL} ${LOCAL} hlist spellin
	(cat british ${SPECIAL} ${LOCAL}) | spellin hlist > hlistb

hstop: stop spellin
	spellin < stop >hstop

clean:
	rm -f ${OBJS} core spell spellin hlista hlistb hlist hstop spellout

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install: ${MAN}
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
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	rm -f ${DESTDIR}/usr/man/cat1/spellin.0
	rm -f ${DESTDIR}/usr/man/cat1/spellout.0
	ln ${DESTDIR}/usr/man/cat1/spell.0 ${DESTDIR}/usr/man/cat1/spellin.0
	ln ${DESTDIR}/usr/man/cat1/spell.0 ${DESTDIR}/usr/man/cat1/spellout.0

lint: ${SRCS}
	lint ${CFLAGS} spell.c
	lint ${CFLAGS} spellin.c
	lint ${CFLAGS} spellout.c

tags: ${SRCS}
	ctags spell.c
	ctags -a spellin.c
	ctags -a spellout.c
	sort -o tags tags
