# Makefile %G%
ALL=	rip.h routed.c query.c trace.c
CFLAGS=	-O
LDFLAGS=

.c.o:
	${CC} -S ${CFLAGS} $*.c
	sed -f ../asm.sed $*.s | ${AS} -o $*.o
	rm -f $*.s

all:	routed query trace

routed: routed.o af.o
	${CC} -o routed ${LDFLAGS} routed.o af.o -ljobs

routed.o: rip.h router.h
af.o: router.h

query:	query.o raddr.o
	${CC} -o query ${LDFLAGS} query.o raddr.o

query.o: rip.h
raddr.o: ../raddr.c
	${CC} -S ${CFLAGS} ../raddr.c
	sed -f ../asm.sed raddr.s | ${AS} -o raddr.o
	rm -f raddr.s

trace:	trace.o
	${CC} -o trace ${LDFLAGS} trace.o

install: ${ALL}
	install -s routed /etc

lint:
	lint -hbxn routed.c af.c

clean:
	rm -f routed *.o core a.out t.c query linterrs trace typescript errs
