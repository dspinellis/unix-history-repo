MAKEPARAM = AS=${AS} CC=${CC} LD=${LD} INCLDIR=${INCLDIR} LIBDIR=${LIBDIR} ULIBDIR=${ULIBDIR} DESTDIR=${DESTDIR}
CFLAGS=	-O
LDFLAGS=
OWNER=root
GROUP=bin
INCLDIR=

DOBJS=	dstat.o

DSOURCES=dstat.c

dstat:	${DOBJS}
	${CC} -o dstat ${LDFLAGS} ${DOBJS}

${DOBJS}: ${INCLDIR)/drreg.h

install: dstat
	echo "install -m 4711 -o ${OWNER} -g ${GROUP} dstat ${DESTDIR}/dstat"
	install -m 4711 -o ${OWNER} -g ${GROUP} dstat ${DESTDIR}/dstat

clean:
	rm -f *.o
	
lint:
	-lint -hbacvx ${CFLAGS} ${SOURCES}
