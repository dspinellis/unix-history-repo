#	@(#)Makefile	5.5 (Berkeley) 5/24/90

SRCS=	mkhosts.c
MAN8=	mkhosts.0
.PATH:	${.CURDIR}

all: mkhosts ${MAN8}

mkhosts: ${LIBC} mkhosts.c
	@echo "Mkhosts requires the dbm version of the hostent(3) routines."
	@echo "If gethosts(3) is not in libc, mkhosts will not be installed."
	-${CC} -o ${.TARGET} ${CFLAGS} ${.CURDIR}/${.PREFIX}.c || \
	    cd ${.CURDIR} && ${MAKE} clean

clean:
	rm -f core mkhosts mkhosts.o

cleandir: clean
	rm -f ${MAN8} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${.ALLSRC}

install: maninstall
	-if [ -f mkhosts ]; then \
	    install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
		mkhosts ${DESTDIR}${BINDIR}; \
	else \
		exit 0; \
	fi

lint: ${SRCS}
	cd ${.CURDIR}; lint ${CFLAGS} ${LINTFLAGS} ${SRCS}

tags: ${SRCS}
	cd ${.CURDIR}; ctags ${SRCS}

.include <bsd.prog.mk>
