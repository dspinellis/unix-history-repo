#	@(#)Makefile	5.7 (Berkeley) %G%

# -DSLOWSYS	invoke xread() for system time quirk on PDP, others?
# -DNOKANJI	default is for Japanese Unix.  undef only for raw
#		parity-marked search capability, not standard w/grep.
# -DCHINESE	for systems using EUC Chinese2 codes
# -Dstrrchr=rindex, -Dstrchr=index	as necessary

PROGS=	egrep old.ucb.grep old.bin.grep old.fgrep old.egrep
CFLAGS+=-Dstrrchr=rindex -Dstrchr=index -DNOKANJI
SRCS=	egrep.c old.ucb.grep.c old.bin.grep.c old.fgrep.c old.egrep.c
LIBC=	/usr/lib/libc.a
MAN1=	grep.0
LINKS=	${BINDIR}/egrep ${BINDIR}/grep ${BINDIR}/egrep ${BINDIR}/fgrep
MLINKS=	grep.1 egrep.1 grep.1 fgrep.1

all: ${PROGS} ${MAN1}

egrep oegrep old.ucb.grep old.bin.grep old.fgrep: ${.PREFIX}.c ${LIBC}
	${CC} -o ${.TARGET} ${CFLAGS} ${.CURDIR}/${.PREFIX}.c

old.egrep: old.egrep.c ${LIBC}
	${CC} -o ${.TARGET} ${CFLAGS} ${.PREFIX}.c

clean:
	rm -f a.out Errs errs mklog core ${PROGS} old.egrep.c

cleandir: clean
	rm -f ${MAN1} ${.CURDIR}/tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS:M-[ID]*} ${.ALLSRC}

install: maninstall

maninstall: beforeinstall

beforeinstall:
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} egrep \
	    ${DESTDIR}${BINDIR}/egrep
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	    old.ucb.grep old.bin.grep old.fgrep old.egrep \
	    ${DESTDIR}/usr/libexec

lint: ${SRCS}
	${LINT} ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

tags: ${SRCS}
	-ctags ${.ALLSRC} -f ${.CURDIR}/tags

.include <bsd.prog.mk>
