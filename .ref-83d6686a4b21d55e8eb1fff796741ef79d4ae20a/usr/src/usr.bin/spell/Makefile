#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.11	(Berkeley)	%G%
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

hlist: spellin
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

DIR=	${DESTDIR}/usr/share/dict
FILES=	hlista hlistb hstop hlist american british connectives \
	propernames stop README words web2 web2a
install: ${MAN}
	-[ -d ${DIR}/special ] || mkdir ${DIR}/special
	install -s -o bin -g bin -m 755 spell ${DESTDIR}/usr/libexec/spell
	install -s -o bin -g bin -m 755 spellin ${DESTDIR}/usr/libexec/spellin
	install -s -o bin -g bin -m 755 spellout ${DESTDIR}/usr/libexec/spellout
	install -c -o bin -g bin -m 755 spell.sh ${DESTDIR}/usr/bin/spell
	install -c -o bin -g bin -m 444 ${FILES} ${DIR}
	install -c -o bin -g bin -m 444 special.4bsd ${DIR}/special/4bsd
	install -c -o bin -g bin -m 444 special.math ${DIR}/special/math
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1
	cp -p -r papers ${DIR}; chmod 755 ${DIR}/papers/runinv
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
