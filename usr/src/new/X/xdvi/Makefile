#
# Makefile for dvi previewer.
#
# $Header: Makefile,v 10.5 86/02/01 15:44:17 tony Rel $
#
DESTDIR=
CONFDIR= /usr/new
INCLUDES= -I../include
CFLAGS= -O ${INCLUDES}
LIBS= ../Xlib/libX.a

OBJS= dv.o table.o

all: xdvi mktable

xdvi: $(OBJS)
	cc $(CFLAGS) -o xdvi $(OBJS) $(LIBS)

dv.o: dvi.h pxl.h

table.c: mktable
	mktable > table.c

mktable: mktable.c
	cc -o mktable mktable.c

install: all
	install xdvi ${DESTDIR}${CONFDIR}

clean:
	-rm -f mktable table.c *.o *.BAK *.CKP xdvi a.out core errs *~
