# Makefile %G%
ALL=	routed
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o
CFLAGS=	-O
DESTDIR=
LDFLAGS=

.c.o:
	${CC} -S ${CFLAGS} $*.c
	sed -f ../asm.sed $*.s | ${AS} -o $*.o
	rm -f $*.s

all:	${ALL}

routed: ${OBJS}
	${CC} -o routed ${LDFLAGS} ${OBJS} -ljobs

${ALL}:	rip.h
${ALL}:	router.h

install: routed
	install -s routed ${DESTDIR}/etc

clean:
	rm -f routed *.o core a.out t.c linterrs typescript errs
