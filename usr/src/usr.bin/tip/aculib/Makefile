#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) 4/3/86
#
# make file for tip device drivers
#
# Current drivers:
#	BIZCOMP
#	USR COURIER (2400 baud)
#	DEC DF02-AC, DF03-AC
#	DEC DN-11/Able Quadracall
#	HAYES and Hayes emulators
#	VENTEL 212+ (w/o echo)
#	VADIC 831 RS232 adaptor
#	VADIC 3451

CFLAGS=	-O -I../
OBJS=	biz22.o biz31.o courier.o df.o dn11.o hayes.o ventel.o v831.o v3451.o

aculib.a: ${OBJS}
	ar cu aculib.a *.o
	ranlib aculib.a

${OBJS}: ../tip.h

clean:
	rm -f aculib.a *.o core errs a.out
