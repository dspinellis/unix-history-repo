#	@(#)Makefile	5.3 (Berkeley) %G%

SRCS=	llib-lc llib-lcurses llib-ldbm llib-lm llib-lmp llib-lplot \
	llib-ltermcap llib-port
LN=	llib-port.ln llib-lc.ln llib-lcurses.ln llib-ldbm.ln llib-lm.ln \
	llib-lmp.ln llib-lplot.ln llib-ltermcap.ln

BINDIR=	/usr/old/libdata/lint
LINKS=	${BINDIR}/llib-ltermcap ${BINDIR}/llib-ltermlib \
	${BINDIR}/llib-ltermcap.ln ${BINDIR}/llib-ltermlib.ln

all llibs: ${LN}

clean cleandir:
	rm -f ${LN}

depend lint tags:

beforeinstall:
	cd ${.CURDIR}; install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${SRCS} \
	    ${DESTDIR}${BINDIR}
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${LN} \
	    ${DESTDIR}${BINDIR}

.include <bsd.prog.mk>

LP1=	/usr/libexec/lintpass1

llib-port.ln: llib-port
	-(cd ${.CURDIR}; ${CPP} -C -Dlint llib-port) | \
	    ${LP1} -puv > llib-port.ln

llib-ldbm.ln: llib-ldbm
	-(cd ${.CURDIR}; ${CPP} -C -Dlint llib-ldbm) | \
	    ${LP1} -uv > llib-ldbm.ln

llib-lm.ln: llib-lm
	-(cd ${.CURDIR}; ${CPP} -C -Dlint llib-lm) | \
	    ${LP1} -uv > llib-lm.ln

llib-lmp.ln: llib-lmp
	-(cd ${.CURDIR}; ${CPP} -C -Dlint llib-lmp) | \
	    ${LP1} -uv > llib-lmp.ln

llib-lplot.ln: llib-lplot
	-(cd ${.CURDIR}; ${CPP} -C -Dlint llib-lplot) | \
	    ${LP1} -uv > llib-lplot.ln

llib-ltermcap.ln: llib-ltermcap
	-(${CPP} -C -Dlint ${.CURDIR}/llib-ltermcap) | \
	    ${LP1} -uv > llib-ltermcap.ln

llib-lc.ln: llib-lc
	-(cd ${.CURDIR}; ${CPP} -C -Dlint llib-lc) | \
	    ${LP1} -v > llib-lc.ln

llib-lcurses.ln: llib-lcurses
	-(cd ${.CURDIR}; ${CPP} -C -Dlint -D__void__=__curses__ \
	    llib-lcurses) | ${LP1} -v > llib-lcurses.ln
