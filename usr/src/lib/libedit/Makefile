#	@(#)Makefile	5.1 (Berkeley) %G%

SRCS=	chared.c common.c el.c emacs.c hist.c history.c key.c map.c \
	parse.c prompt.c read.c refresh.c search.c sig.c term.c \
	tokenizer.c tty.c vi.c
CLEANFILES+=common.h emacs.h fcns.h help.h vi.h
CFLAGS+=-I. -I${.CURDIR}
CFLAGS+=#-DDEBUG_TTY -DDEBUG_KEY -DDEBUG_READ -DDEBUG -DDEBUG_REFRESH
CFLAGS+=#-DDEBUG_PASTE

vi.h: vi.c makelist
	(cd ${.CURDIR} && sh makelist -h vi.c) > $@

emacs.h: emacs.c makelist
	(cd ${.CURDIR} && sh makelist -h emacs.c) > $@

common.h: common.c makelist
	(cd ${.CURDIR} && sh makelist -h common.c) > $@

AHDR=vi.h emacs.h common.h 
fcns.h: ${AHDR} makelist
	sh ${.CURDIR}/makelist -fh ${AHDR} > $@

fcns.c: ${AHDR} fcns.h makelist
	(cd ${.CURDIR} && sh makelist -fc ${AHDR}) > $@

ASRC=vi.c emacs.c common.c
help.c: ${ASRC} makelist 
	(cd ${.CURDIR} && sh makelist -bc ${ASRC}) > $@

help.h: makelist
	sh ${.CURDIR}/makelist -bh > $@

.depend: vi.h emacs.h common.h fcns.h help.h

beforeinstall:
        -cd ${.CURDIR}; cmp -s histedit.h ${DESTDIR}/usr/include/histedit.h || \
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 histedit.h \
	${DESTDIR}/usr/include


.include <bsd.lib.mk>
