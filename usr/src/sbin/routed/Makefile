#	@(#)Makefile	4.6 (Berkeley) %G%
#
ALL=	routed
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o inet.o
CFLAGS=
C2=	/lib/c2
ASMSCRIPT= /sys/machine/asm.sed
DESTDIR=
LDFLAGS=

all:	${ALL}

routed: ${OBJS}
	${CC} -o routed ${LDFLAGS} ${OBJS}

install: routed
	install -s routed ${DESTDIR}/etc

# The following can be deleted where not appropriate to use the kernel's
# inline code expansions.
.c.o:
	${CC} -S ${CFLAGS} $*.c
	${C2} $*.s | sed -f ${ASMSCRIPT} | ${AS} -o $*.o
	rm -f $*.s

clean:
	rm -f routed *.o core a.out t.c linterrs typescript errs
