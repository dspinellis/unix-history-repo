#
#	Copyright 1985, Massachusetts Institute of Technology.
#	$Header: Makefile,v 10.4 86/02/01 16:18:36 tony Rel $
#

DESTDIR =
CONFDIR = /usr/new
INCLUDES = -I../include

CFLAGS = -O $(INCLUDES)
XLIB = ../Xlib/libX.a
OBJS = gedit.o gcmd.o garc.o gx.o

all:	xgedit

xgedit:	$(OBJS)
	cc -o xgedit $(OBJS) -lg -lm $(XLIB)

$(OBJS): gedit.h

install: xgedit
	install -c -s -m 755 xgedit ${DESTDIR}${CONFDIR}

clean: 
	rm -f *.bak *.o *~ xgedit \#*

lint:
	lint -cah $(INCLUDES) *.c -lX
