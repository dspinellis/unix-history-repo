#
#	Makefile for X window system terminal emulator.
#

DESTDIR=
# We put xterm in /etc so you can run on partial boot.  A link is put
# in CONFDIR so normal search paths will find xterm.
# 
# For the 4.3 distribution, the executable is put in /usr/new instead.
#
CONFDIR= /usr/new
INCLUDES= -I../include
#CFLAGS = -g -DJUMPSCROLL -DMODEMENU
CFLAGS = -O -DJUMPSCROLL -DMODEMENU ${INCLUDES}

.SUFFIXES: .o .h .c

OBJS =	main.o input.o charproc.o cursor.o util.o tabs.o buf.o ansi.o \
	screen.o button.o Tplot.o

all: xterm resize

bup:	Makefile ptyx.h ansi.c buf.c button.c chartable.h \
		esctable.h charproc.c cursor.c input.c \
		main.c screen.c tabs.c 	Tplot.c util.c

$(OBJS): ptyx.h

charproc.o: chartable.h esctable.h

main.o: icon.ic icon_mask.ic

xterm: $(OBJS) ../Xlib/libX.a ../XMenu/libXMenu.a
	$(CC) $(CFLAGS) -o xterm $(OBJS) ../XMenu/libXMenu.a ../Xlib/libX.a -ltermcap

resize: resize.o
	$(CC) $(CFLAGS) -o resize resize.o -lc -ltermcap

install: all
#	install -m 4755 xterm ${DESTDIR}/etc
	install -m 4755 xterm ${DESTDIR}${CONFDIR}
#	rm -f ${DESTDIR}${CONFDIR}/xterm
#	ln -s /etc/xterm ${DESTDIR}${CONFDIR}/xterm
	install resize ${DESTDIR}${CONFDIR}

clean:
	rm -f xterm resize *.o a.out core errs gmon.out *.bak *~

print:
	lpr -Pln ptyx.h ansi.c buf.c button.c chartable.h \
		esctable.h charproc.c cursor.c input.c \
		main.c screen.c tabs.c 	Tplot.c util.c
