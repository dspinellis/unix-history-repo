#	Makefile	4.3	83/07/08
#
SRCS=	client.c bind.c server.c misc.c
OBJS=	client.o bind.o server.o misc.o
CFLAGS=	-O -DDEBUG
DESTDIR=

.c.o:
	${CC} -c ${CFLAGS} -p $*.c
	ld -x -r -o profiled/$*.o $*.o
	${CC} -c ${CFLAGS} $*.c
	ld -x -r $*.o
	mv a.out $*.o

libcr libcr_p: ${OBJS}
	@echo building profiled libcr
	@cd profiled; ar cr ../libcr_p ${OBJS}
	@echo building normal libcr
	@ar cr libcr ${OBJS}

lint:	${SRCS}
	lint -hux ${SRCS}

# beware, the compiler expects courier.h to be in /usr/include
install: libcr libcr_p
	install -c -m 644 courier.h ${DESTDIR}/usr/include/courier.h
	install -m 644 libcr ${DESTDIR}/usr/new/lib/libcr.a
	ranlib ${DESTDIR}/usr/new/lib/libcr.a
	install -m 644 libcr_p ${DESTDIR}/usr/new/lib/libcr_p.a
	ranlib ${DESTDIR}/usr/new/lib/libcr_p.a

clean:
	-rm -f ${OBJS} profiled/*.o libcr libcr_p
