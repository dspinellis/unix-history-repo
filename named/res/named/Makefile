#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	6.4 (Berkeley) 6/6/87
#

OBJS=	gethostnamadr.o sethostent.o

SRCS=	gethostnamadr.c sethostent.c

CFLAGS= -O ${DEFS} 
TAGSFILE= tags
DESTDIR=

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
	@echo "building normal hostlib"
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

gethostnamadr.o: gethostnamadr.c /usr/include/sys/param.h
gethostnamadr.o: /usr/include/sys/types.h /usr/include/signal.h
gethostnamadr.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
gethostnamadr.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
gethostnamadr.o: /usr/include/ctype.h /usr/include/netdb.h /usr/include/stdio.h
gethostnamadr.o: /usr/include/errno.h /usr/include/arpa/inet.h
gethostnamadr.o: /usr/include/arpa/nameser.h /usr/include/resolv.h
sethostent.o: sethostent.c /usr/include/sys/types.h /usr/include/arpa/nameser.h
sethostent.o: /usr/include/netinet/in.h /usr/include/resolv.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
