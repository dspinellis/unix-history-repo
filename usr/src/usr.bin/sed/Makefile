
PROG=	sed
SRCS=	sed.c utils.c regex.c getopt.c getopt1.c
CFLAGS+=-I${.CURDIR} # -DSTDC_HEADERS
NOMAN=noman

sed.o regex.o: regex.h
sed.o getopt1.o: getopt.h

.include <bsd.prog.mk>
