#
# Copyright (c) 1987 Regents of the University of California.
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
#	@(#)Makefile	5.5 (Berkeley) 6/29/88
#
LIBC=	/lib/libc.a
RDIST=	/usr/ucb/rdist
CFLAGS=	-O -DRDIST=\"${RDIST}\"
SRCS=	docmd.c expand.c gram.y lookup.c main.c server.c
OBJS=	docmd.o expand.o gram.o lookup.o main.o server.o

all: rdist

rdist: ${OBJS} ${LIBC}
	${CC} -o $@ ${CFLAGS} ${OBJS}

clean: FRC
	rm -f ${OBJS} core rdist

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -s -o root -g bin -m 4751 rdist ${DESTDIR}${RDIST}

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

docmd.o: docmd.c defs.h /usr/include/stdio.h /usr/include/ctype.h
docmd.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/grp.h
docmd.o: /usr/include/sys/param.h /usr/include/sys/types.h
docmd.o: /usr/include/signal.h /usr/include/machine/trap.h
docmd.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
docmd.o: /usr/include/sys/dir.h /usr/include/sys/stat.h /usr/include/sys/time.h
docmd.o: /usr/include/time.h /usr/include/netinet/in.h /usr/include/setjmp.h
docmd.o: /usr/include/netdb.h
expand.o: expand.c defs.h /usr/include/stdio.h /usr/include/ctype.h
expand.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/grp.h
expand.o: /usr/include/sys/param.h /usr/include/sys/types.h
expand.o: /usr/include/signal.h /usr/include/machine/trap.h
expand.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
expand.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
expand.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h
gram.o: gram.y defs.h /usr/include/stdio.h /usr/include/ctype.h
gram.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/grp.h
gram.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
gram.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
gram.o: /usr/include/machine/endian.h /usr/include/sys/dir.h
gram.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
gram.o: /usr/include/netinet/in.h
lookup.o: lookup.c defs.h /usr/include/stdio.h /usr/include/ctype.h
lookup.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/grp.h
lookup.o: /usr/include/sys/param.h /usr/include/sys/types.h
lookup.o: /usr/include/signal.h /usr/include/machine/trap.h
lookup.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
lookup.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
lookup.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h
main.o: main.c defs.h /usr/include/stdio.h /usr/include/ctype.h
main.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/grp.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
main.o: /usr/include/machine/endian.h /usr/include/sys/dir.h
main.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
main.o: /usr/include/netinet/in.h
server.o: server.c defs.h /usr/include/stdio.h /usr/include/ctype.h
server.o: /usr/include/errno.h /usr/include/pwd.h /usr/include/grp.h
server.o: /usr/include/sys/param.h /usr/include/sys/types.h
server.o: /usr/include/signal.h /usr/include/machine/trap.h
server.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
server.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
server.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/netinet/in.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
