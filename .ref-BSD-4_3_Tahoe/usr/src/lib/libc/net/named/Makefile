#
# Copyright (c) 1988 Regents of the University of California.
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
#	@(#)Makefile	6.7 (Berkeley) 6/27/88
#
DEFS=	-DLIBC_SCCS
CFLAGS= -O ${DEFS} 
SRCS=	gethostnamadr.c sethostent.c
OBJS=	gethostnamadr.o sethostent.o
TAGSFILE= tags

.c.o:
	@${CC} -p -c ${CFLAGS} $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../../library; rm -f ${OBJS})
	(cd ../../profiled; rm -f ${OBJS})
	ln ${OBJS} ../../library
	(cd profiled; ln ${OBJS} ../../../profiled)

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend:
	mkdep ${CFLAGS} ${SRCS}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

gethostnamadr.o: gethostnamadr.c /usr/include/sys/param.h
gethostnamadr.o: /usr/include/sys/types.h /usr/include/signal.h
gethostnamadr.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
gethostnamadr.o: /usr/include/machine/endian.h /usr/include/sys/socket.h
gethostnamadr.o: /usr/include/netinet/in.h /usr/include/ctype.h
gethostnamadr.o: /usr/include/netdb.h /usr/include/stdio.h /usr/include/errno.h
gethostnamadr.o: /usr/include/arpa/inet.h /usr/include/arpa/nameser.h
gethostnamadr.o: /usr/include/resolv.h
sethostent.o: sethostent.c /usr/include/sys/types.h /usr/include/arpa/nameser.h
sethostent.o: /usr/include/netinet/in.h /usr/include/resolv.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
