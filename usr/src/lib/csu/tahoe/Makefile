#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
SRCS=	gmon.c mon.c
OBJS=	crt0.o mcrt0.o gcrt0.o
TAGSFILE=tags

.s.o:
	${AS} -o $*.o $*.s
	ld -x -r $*.o
	mv a.out $*.o

all link: ${OBJS}

mcrt0.o: moncrt0.o mon.o
	ld -x -r -o $@ moncrt0.o mon.o

gcrt0.o: moncrt0.o gmon.o
	ld -x -r -o $@ moncrt0.o gmon.o

moncrt0.o: mcrt0.s
	${AS} -o $@ mcrt0.s

mon.o: mon.c
	${CC} -S ${CFLAGS} mon.c
	ex - mon.s < mon.ex
	${AS} -o $@ mon.s

gmon.o: gmon.c
	${CC} -S ${CFLAGS} gmon.c
	ex - gmon.s < mon.ex
	${AS} -o $@ gmon.s

install:
	install -o bin -g bin -m 444 crt0.o ${DESTDIR}/lib/crt0.o
	install -o bin -g bin -m 444 mcrt0.o ${DESTDIR}/lib/mcrt0.o
	install -o bin -g bin -m 444 gcrt0.o ${DESTDIR}/usr/lib/gcrt0.o

clean:
	rm -f *.o *mon.s a.out core ${TAGSFILE}

depend:
