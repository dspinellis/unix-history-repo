#
# Copyright 1985, Massacusetts Institute of Technology
#
DESTDIR=
CONFDIR= /usr/new
INCLUDES= -I../include
CFLAGS = -O ${INCLUDES} -DNOINLINE
XLIB= ../Xlib/libX.a
C2=/lib/c2

# if you are on a 68000 or a Vax, uncomment the next three lines
# and remove -DNOINLINE from CFLAGS above
#.c.o:
#	${CC} ${CFLAGS} -S $*.c
#	${C2} $*.s | ../inline/inline | ${AS} -o $*.o
#	rm -f $*.s

all: xpr

install: all
	install xpr ${DESTDIR}${CONFDIR}
	install -c xdpr.script ${DESTDIR}${CONFDIR}/xdpr

xpr: xpr.o ../Xlib/Xlib.h
	${CC} ${CFLAGS} -o xpr xpr.o

xpr.o: lncmd.h

clean:
	rm -f xpr *~ *.o \#*

