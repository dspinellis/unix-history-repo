#	Makefile for Xerox Routing Information Protocol
#	Daemon
#
ALL=	xnsrouted
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o
CFLAGS= -g
C2=	/lib/c2
ASMSCRIPT= /sys/machine/asm.sed
DESTDIR=
LDFLAGS= -g

all:	${ALL}

xnsrouted: ${OBJS}
	${CC} -o xnsrouted ${LDFLAGS} ${OBJS}

install: xnsrouted
	install -s xnsrouted ${DESTDIR}/etc

# The following can be deleted where not appropriate to use the kernel's
# inline code expansions.
#.c.o:
#	${CC} -S ${CFLAGS} $*.c
#	${C2} $*.s | sed -f ${ASMSCRIPT} | ${AS} -o $*.o
#	rm -f $*.s

clean:
	rm -f xnsrouted *.o core a.out t.c linterrs typescript errs
