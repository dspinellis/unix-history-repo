# Makefile 10/21/82
ALL=	rip.h routed.c query.c trace.c
CFLAGS=	-O
DESTDIR=
LDFLAGS=

.c.o:
	${CC} -S ${CFLAGS} $*.c
	sed -f ../asm.sed $*.s | ${AS} -o $*.o
	rm -f $*.s

all:	routed

routed: routed.o af.o
	${CC} -o routed ${LDFLAGS} routed.o af.o -ljobs

routed.o query.o: rip.h
routed.o af.o: router.h

# test programs and utilities
query:	query.o
	${CC} -o query ${LDFLAGS} query.o

trace:	trace.o
	${CC} -o trace ${LDFLAGS} trace.o

install: routed
	install -s routed ${DESTDIR}/etc

lint:
	lint -hbxn routed.c af.c

clean:
	rm -f routed *.o core a.out t.c query linterrs trace typescript errs
