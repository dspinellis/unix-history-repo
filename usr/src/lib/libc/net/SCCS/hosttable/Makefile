
#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#

OBJS=	gethostnamadr.o 
CFLAGS=	-O ${DEFS}

gethostnamadr.o: 
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out ../profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out ../$*.o

netlib netlib_p: ${OBJS} 
	@echo "building profiled netlib"
	@cd ../profiled; ar ru ../netlib_p ${TARGETOBJS}
	@echo "building normal netlib"
	@ar ru ../netlib ${TARGETOBJS}
	
clean:
	rm -f ${OBJS} errs a.out core 
