#	@(#)Makefile	4.2	(Berkeley)	86/05/13
#
# Copyright (c) 1986 Regents of the University of California
#	All Rights Reserved
#

ALL=	nsquery.c nstest.c
CFLAGS=	-O
DESTDIR=
LDFLAGS=

all:	nsquery nstest nslookup

# test programs and utilities
nsquery:	nsquery.o
	${CC} -o nsquery ${LDFLAGS} nsquery.o

nstest:	nstest.o
	@echo "The resolver routines in libc must be compiled with the -DDEBUG"
	@echo "option or nstest will not have any output."
	${CC} -o nstest ${LDFLAGS} nstest.o

nslookup:
	cd ns.lookup/src; make
	cp ns.lookup/src/nslookup .

install:
	@echo Nothing to install

clean:
	rm -f *.o core a.out nsquery nslookup linterrs nstest typescript errs r
	cd ns.lookup/src; make clean
