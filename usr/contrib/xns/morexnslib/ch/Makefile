#CFLAGS=	-g -DDEBUG
CFLAGS= -O
LDLIB=${DESTDIR}/usr/local/lib
CRLIB=${DESTDIR}/usr/new/lib/xnscourier
OUT=${LDLIB}/libcourier.a
OUT_P=${LDLIB}/libcourier_p.a
SRCS= CHlookup.c CHenumerate.c CHenumalias.c CHstrtoname.c
OBJS= CHlookup.o CHenumerate.o CHenumalias.o CHstrtoname.o
RCSREV=
RCSNAME=current
BINDIR=${DESTDIR}/usr/new

.c.o:
	${CC} -c ${CFLAGS} -p $*.c -I/usr/include/xnscourier
	mv $*.o profiled/$*.o
	${CC} -c ${CFLAGS} $*.c -I/usr/include/xnscourier

install: ${OBJS}
	ar rv ${OUT} ${OBJS}
	cd profiled; ar rv ${OUT_P} ${OBJS}
	ranlib ${OUT} ${OUT_P}
	cp CH.h /usr/include/xnscourier
	cp clearinghouse.addresses ${CRLIB}

all:	${OBJS} ${TESTS}

lint:	${SRCS}
	lint -hux ${SRCS}

tags:	${SRCS}
	ctags ${SRCS}

clean:
	-rm -f Clearinghouse2.h Clearinghouse2_defs.h \
		*.BAK *.CKP *.o profiled/*.o

CHenumerate.o CHenumalias.o: Clearinghouse2_defs.h

CHlookup.o: CHlookup.c
	${CC} -c -p ${CFLAGS} -D'CHADDRS="${CRLIB}/clearinghouse.addresses"' \
		CHlookup.c
	mv CHlookup.o profiled/CHlookup.o
	${CC} -c ${CFLAGS} -D'CHADDRS="${CRLIB}/clearinghouse.addresses"' \
		CHlookup.c

Clearinghouse2_defs.h: ../../courierlib/Clearinghouse2_defs.h
	cp ../../courierlib/Clearinghouse2_defs.h .

rcs:
	rcs -l RCS/*
	cat ../../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
