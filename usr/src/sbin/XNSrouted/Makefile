#	Makefile for Xerox Routing Information Protocol
#	Daemon
#
ALL=	XNSrouted
OBJS=	af.o if.o input.o main.o output.o startup.o tables.o timer.o \
	trace.o
CFLAGS= -g
C2=	/lib/c2
ASMSCRIPT= /sys/machine/asm.sed
DESTDIR=
LDFLAGS= -g

all:	${ALL}

XNSrouted: ${OBJS}
	${CC} -o XNSrouted ${LDFLAGS} ${OBJS}

install: XNSrouted
	install -s XNSrouted ${DESTDIR}/etc

# The following can be deleted where not appropriate to use the kernel's
# inline code expansions.
#.c.o:
#	${CC} -S ${CFLAGS} $*.c
#	${C2} $*.s | sed -f ${ASMSCRIPT} | ${AS} -o $*.o
#	rm -f $*.s

clean:
	rm -f XNSrouted *.o core a.out t.c linterrs typescript errs
