#	@(#)Makefile	4.16 (Berkeley) %G%

LIBC=	/usr/lib/libc.a
SRCS=	spell.c spellin.c spellout.c
MAN1=	spell.0
MLINKS=	spell.1 spellin.1 spell.1 spellout.1
SPECIAL=${.CURDIR}/special.4bsd

all: spell hlist hlista hlistb hstop spellin spellout ${MAN1}

spell spellin spellout: ${LIBC}
	${CC} ${CFLAGS} ${.CURDIR}/${.PREFIX}.c -o ${.TARGET}

hlist: spellin
	spellin < /usr/share/dict/words > hlist

hlista: american ${SPECIAL} hlist spellin
	(cat ${.CURDIR}/american ${SPECIAL}) | spellin hlist > hlista

hlistb: british ${SPECIAL} hlist spellin
	(cat ${.CURDIR}/british ${SPECIAL}) | spellin hlist > hlistb

hstop: stop spellin
	spellin < ${.CURDIR}/stop > hstop

clean:
	rm -f a.out Errs errs mklog core spell spellin hlista \
	    hlistb hlist hstop spellout

cleandir: clean
	rm -f ${MAN1} ${.CURDIR}/tags .depend

depend: .depend
.depend: ${SRCS}
	mkdep -p ${CFLAGS:M-[ID]*} ${.ALLSRC}

DIR=	${DESTDIR}/usr/share/dict
FILES=	${.CURDIR}/american ${.CURDIR}/british hlist hlista hlistb hstop \
	${.CURDIR}/stop

install: maninstall
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    spellin spellout ${DESTDIR}/usr/bin
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    spell ${DESTDIR}/usr/libexec
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    ${.CURDIR}/spell.sh ${DESTDIR}/usr/bin/spell
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${FILES} ${DIR}
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/special.4bsd \
	    ${DIR}/special/4bsd
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${.CURDIR}/special.math \
	    ${DIR}/special/math

lint: ${SRCS}
	cd ${.CURDIR}; ${LINT} ${LINTFLAGS} ${CFLAGS} spell.c
	cd ${.CURDIR}; ${LINT} ${LINTFLAGS} ${CFLAGS} spellin.c
	cd ${.CURDIR}; ${LINT} ${LINTFLAGS} ${CFLAGS} spellout.c

tags: ${SRCS}
	cd ${.CURDIR}; ctags ${SRCS}

.include <bsd.prog.mk>
