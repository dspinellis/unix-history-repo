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
#	@(#)Makefile	5.10 (Berkeley) 9/20/88
#
DEFS=	-DLIBC_SCCS
FLAGS=	-I../../tahoe ${DEFS}
CFLAGS= -O ${FLAGS}
SRCS=	htonl.s htons.s ntohl.s ntohs.s
OBJS=	htonl.o htons.o ntohl.o ntohs.o
TAGSFILE=tags

.s.o:
	@/lib/cpp -E ${FLAGS} -DPROF $*.s | ${AS} -o $*.o
	@ld -X -o profiled/$*.o -r $*.o
	/lib/cpp -E ${FLAGS} $*.s | ${AS} -o $*.o
	@ld -x -r $*.o
	@mv a.out $*.o

all: ${OBJS}

link: ${OBJS}
	(cd ../../library; rm -f ${OBJS})
	(cd ../../profiled; rm -f ${OBJS})
	ln ${OBJS} ../../library
	(cd profiled; ln ${OBJS} ../../../profiled)

tags:
	(P=`pwd`; \
	egrep "^ENTRY(.*)|^SYSCALL(.*)" ${SRCS} | /bin/sed \
"s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 $$P/\1 /^\2(\3\4$$/;" \
		>> ${TAGSFILE})

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

cleandir: clean

depend:
	mkdep ${CFLAGS} ${SRCS}
