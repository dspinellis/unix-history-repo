#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#

OBJS=	gethostbyname.o gethostbyaddr.o 
CFLAGS=	-O ${DEFS}

gethostbyname.o: 
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out ../$*.o

gethostbyaddr.o:
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out ../$*.o

clean:
	rm -f ${OBJS} errs a.out core 
