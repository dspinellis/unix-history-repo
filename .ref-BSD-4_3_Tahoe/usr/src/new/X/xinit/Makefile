#
#	$Header: Makefile,v 10.4 86/02/02 14:58:34 jg Rel $
#

# Copyright 1986, Massachusetts Institute of Technology.

DESTDIR=
CONFDIR= /usr/new
INCLUDES= -I../include

CFLAGS= -O $(INCLUDES)

all: xinit

install:
	install -m 4755 xinit ${DESTDIR}${CONFDIR}

clean: 
	rm -f xinit.o xinit *~* *.bak core \#*

xinit: xinit.o
	cc $(CFLAGS) xinit.o -o xinit
