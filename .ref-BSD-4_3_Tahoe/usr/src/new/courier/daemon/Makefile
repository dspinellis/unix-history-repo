#	Makefile	4.1	83/07/03
#
OBJS=	courierd.o
CFLAGS=	-O
DESTDIR=

courierd: ${OBJS}
	${CC} ${CFLAGS} -o courierd ${OBJS}

install: courierd
	install -s courierd ${DESTDIR}/usr/new/lib/courier/courierd

clean:
	-rm -f ${OBJS} courierd
