
#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) %G%
#

OBJS=	gethostnamadr.o gethostent.o
SRCS=	gethostnamadr.c gethostent.c
CFLAGS=	-O ${DEFS}

.c.o: 
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

hostlib hostlib_p: ${OBJS} 
	@echo "building profiled hostlib"
	@cd profiled; ar cru ../hostlib_p ${OBJS}
	@echo "building normal netlib"
	@ar cru hostlib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o errs a.out core hostlib hostlib_p profiled/*.o tags

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

gethostnamadr.o: gethostnamadr.c /usr/include/stdio.h /usr/include/netdb.h
gethostnamadr.o: /usr/include/sys/file.h /usr/include/ndbm.h
gethostnamadr.o: /usr/include/ctype.h
gethostent.o: gethostent.c /usr/include/stdio.h /usr/include/sys/types.h
gethostent.o: /usr/include/sys/socket.h /usr/include/netdb.h
gethostent.o: /usr/include/ctype.h /usr/include/ndbm.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
