#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
CFLAGS=	-O
STDSRCS=ftime.c gtty.c nice.c pause.c rand.c stty.c tell.c \
	times.c utime.c vlimit.c vtimes.c
STD=	ftime.o gtty.o nice.o pause.o rand.o stty.o tell.o \
	times.o utime.o vlimit.o vtimes.o
# these should just be pitched, but in the interest of compatibility...
TRASHSRC=getpw.c
TRASH=	getpw.o 
SRCS=	${STDSRCS} ${TRASHSRCS}
OBJS=	${STD} ${TRASH}
TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

compat-4.1lib compat-4.1lib_p: ${OBJS}
	@echo "building profiled compat-4.1lib"
	@cd profiled; ar cru ../compat-4.1lib_p ${OBJS}
	@echo "buiding normal compat-4.1lib"
	@ar cru compat-4.1lib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f compat-4.1lib compat-4.1lib_p ${OBJS} profiled/*.o tags
