#
# makefile for performance monitoring program.
#
DESTDIR =
CONFDIR = /usr/new
INCLUDES= -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
ALL=input.c xperfmon.c
ALLOBS=input.o xperfmon.o

all: xperfmon

xperfmon: $(ALLOBS) ../Xlib/Xlib.h
	cc $(CFLAGS) $(ALLOBS) ${XLIB} -o xperfmon

clean:
	rm -f xperfmon *.o a.out core errs gmon.out *.bak *~

install: all
	install -s -m 2755 -g kmem xperfmon ${DESTDIR}${CONFDIR}
