#	@(#)Makefile	5.11 (Berkeley) 6/8/90

LIB=	I77
SRCS=	backspace.c c_dfe.c c_iio.c c_sfe.c close.c dofio.c dolio.c douio.c \
	due.c endfile.c err.c f77_abort.c f_errlist.c fmt.c fmtlib.c \
	inquire.c lread.c lwrite.c open.c rdfe.c rdfmt.c rewind.c rfi.c \
	rsfe.c rsli.c rsnmle.c sue.c util.c wdfe.c wfi.c wrtfmt.c wsfe.c \
	wsli.c wsnmle.c 

LDADD=		Version.o
CLEANFILES+=	Version.c Version.o libI66.o mkvers

all: Version.o libI66.o

Version.o: Version.c
	${CC} -c Version.c

Version.c: mkvers ${SRCS}
	mkvers libI77.a ${.ALLSRC:M*.c} > ${.TARGET}

mkvers: mkvers.c
	${CC} ${CFLAGS} ${.CURDIR}/mkvers.c -o ${.TARGET}

f_errlist.o f_errlist.po: f_errlist.c
	${CC} ${CFLAGS} -c ${.CURDIR}/f_errlist.c -o ${.TARGET}

beforeinstall:
	install -o ${BINOWN} -g ${BINGRP} -m ${LIBMODE} libI66.o \
	    ${DESTDIR}/usr/lib/libI66.a

.include <bsd.lib.mk>

.c.o:
	${CC} ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}

.c.po:
	${CC} -p ${CFLAGS} -c ${.IMPSRC} -o ${.TARGET}
	@${LD} -x -r ${.TARGET}
	@mv a.out ${.TARGET}
