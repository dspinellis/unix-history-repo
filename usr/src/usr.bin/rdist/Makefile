#
# Copyright (c) 1987 Regents of the University of California.
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

docmd.o: docmd.c
docmd.o:defs.h
docmd.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
docmd.o: /usr/include/pwd.h /usr/include/grp.h /usr/include/sys/param.h
docmd.o: /usr/include/sys/types.h /usr/include/signal.h
docmd.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
docmd.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
docmd.o: /usr/include/netinet/in.h /usr/include/setjmp.h /usr/include/netdb.h
expand.o: expand.c
expand.o:defs.h
expand.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
expand.o: /usr/include/pwd.h /usr/include/grp.h /usr/include/sys/param.h
expand.o: /usr/include/sys/types.h /usr/include/signal.h
expand.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
expand.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
expand.o: /usr/include/netinet/in.h
gram.o: gram.y
gram.o:defs.h
gram.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
gram.o: /usr/include/pwd.h /usr/include/grp.h /usr/include/sys/param.h
gram.o: /usr/include/sys/types.h /usr/include/signal.h
gram.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
gram.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
gram.o: /usr/include/netinet/in.h
lookup.o: lookup.c
lookup.o:defs.h
lookup.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
lookup.o: /usr/include/pwd.h /usr/include/grp.h /usr/include/sys/param.h
lookup.o: /usr/include/sys/types.h /usr/include/signal.h
lookup.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
lookup.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
lookup.o: /usr/include/netinet/in.h
main.o: main.c
main.o:defs.h
main.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
main.o: /usr/include/pwd.h /usr/include/grp.h /usr/include/sys/param.h
main.o: /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
main.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
main.o: /usr/include/netinet/in.h
server.o: server.c
server.o:defs.h
server.o: /usr/include/stdio.h /usr/include/ctype.h /usr/include/errno.h
server.o: /usr/include/pwd.h /usr/include/grp.h /usr/include/sys/param.h
server.o: /usr/include/sys/types.h /usr/include/signal.h
server.o: /usr/include/machine/machparam.h /usr/include/sys/dir.h
server.o: /usr/include/sys/stat.h /usr/include/sys/time.h /usr/include/time.h
server.o: /usr/include/netinet/in.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
