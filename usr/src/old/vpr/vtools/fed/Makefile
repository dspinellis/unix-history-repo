#
#	Makefile	4.1	83/03/09
#
CFLAGS=	-O
SRCS=	fed.h \
	fed.c subr.c io.c
OBJS=	fed.o subr.o io.o
LIBS=	-lm -l2648
VGRIND=	csh /usr/ucb/vgrind
DESTDIR=

fed:	${OBJS} /usr/lib/lib2648.a
	${CC} -o fed ${CFLAGS} ${OBJS} ${LIBS}

tags:	/tmp
	ctags ${SRCS}

print:
	print ${SRCS}

vgrind:
	tee index < /dev/null
	${VGRIND} -h fed ${SRCS}
	${VGRIND} -x index

lint:
	lint ${SRCS} > lint.out

clean:
	rm -f fed ${OBJS} errs core trace

install: fed
	install -s fed ${DESTDIR}/usr/ucb/fed
