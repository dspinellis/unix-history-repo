#CFLAGS=	-g -DDEBUG
CFLAGS= -O
LDLIB=${DESTDIR}/usr/local/lib
OUT=${LDLIB}/libcourier.a
OUT_P=${LDLIB}/libcourier_p.a
SRCS= simpleauth.c getXNSpass.c
OBJS= simpleauth.o getXNSpass.o
RCSREV=
RCSNAME=current

.c.o:
	${CC} -c ${CFLAGS} -p $*.c
	mv $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c

install: ${OBJS}
	ar rv ${OUT} ${OBJS}
	cd profiled; ar rv ${OUT_P} ${OBJS}
	ranlib ${OUT} ${OUT_P}

all:	${OBJS}

lint:	${SRCS}
	lint -hux ${SRCS}

tags:	${SRCS}
	ctags ${SRCS}

clean:
	-rm -f *.BAK *.CKP *.o profiled/*.o

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

