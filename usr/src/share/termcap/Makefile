#	@(#)Makefile	5.10 (Berkeley) %G%

# reorder gives an editor command for most common terminals
# (in reverse order from n'th to 1'st most commonly used)
# to move them to the front of termcap
#
MAN5=	termcap.0

all: termcap ${MAN5}

termcap: reorder termcap.src
	ex - ${.CURDIR}/termcap.src < ${.CURDIR}/reorder > /dev/null

clean:
	rm -f termcap

cleandir: clean
	rm -f ${MAN}

install: maninstall
	install -o ${BINOWN} -g ${BINGRP} -m 444 termcap \
	     ${DESTDIR}${BINDIR}/misc
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 map3270 \
	     ${DESTDIR}${BINDIR}/misc

depend lint tags:

.include <bsd.prog.mk>
