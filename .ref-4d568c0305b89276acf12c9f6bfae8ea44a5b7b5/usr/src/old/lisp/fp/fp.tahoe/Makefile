#	@(#)Makefile	5.4 (Berkeley) %G%

CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	fp.c
OBJS=
MAN=

all: fp

fp: ${LIBC}
	${CC} -o $@ ${CFLAGS} $@.c

clean:
	rm -f ${OBJS} core fp

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep -p ${CFLAGS} ${SRCS}

install:
	install -s -o bin -g bin -m 755 fp ${DESTDIR}/usr/bin

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
