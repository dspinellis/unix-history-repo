#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) 8/9/85
#
# Make a Yacc library
#
DESTDIR=
CFLAGS=-O 

liby.a:	libmai.o libzer.o
	ar rv liby.a libmai.o libzer.o
	ranlib liby.a

install: liby.a
	install liby.a ${DESTDIR}/usr/lib/liby.a
	ranlib ${DESTDIR}/usr/lib/liby.a

clean:
	rm -f *.o *.a errs
