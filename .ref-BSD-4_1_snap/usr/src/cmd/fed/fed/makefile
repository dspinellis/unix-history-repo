DEB=
CFLAGS=	-O ${DEB}
OBJS=	fed.o subr.o io.o
LIBS=	-lm -l2648
VGRIND=	csh /usr/ucb/vgrind
SRCS=	fed.h \
	fed.c subr.c io.c
fed:	${OBJS} /usr/lib/lib2648.a
	${CC} ${CFLAGS} ${OBJS} -o fed ${LIBS}
tags:	/tmp
	ctags ${SRCS}
print:
	print ${SRCS}
vgrind:
	tee index </dev/null
	${VGRIND} -h fed ${SRCS}
	${VGRIND} -x index
lint:
	lint ${SRCS} >lint.out
clean:
	rm fed ${OBJS} errs core trace
install:	fed
	strip fed
	cp fed /usr/ucb/fed
	chmod 755 /usr/ucb/fed
