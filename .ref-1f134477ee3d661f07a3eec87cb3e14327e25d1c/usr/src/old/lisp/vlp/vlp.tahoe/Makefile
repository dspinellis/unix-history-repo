#	@(#)Makefile	5.3 (Berkeley) %G%

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	vlp.c
OBJS=
MAN=

all: vlp

vlp: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f ${OBJS} core vlp

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install:
	install -s -o bin -g bin -m 755 vlp ${DESTDIR}/usr/bin

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
