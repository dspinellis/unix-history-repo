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
#	from @(#)Makefile	5.16 (Berkeley) 3/14/88
#
SRCS=	herror.c res_comp.c res_debug.c res_init.c res_mkquery.c res_query.c \
	res_send.c named/gethostnamadr.c named/sethostent.c \
	mktemp.c strcasecmp.c
OBJS=	herror.o res_comp.o res_debug.o res_init.o res_mkquery.o res_query.o \
	res_send.o gethostnamadr.o sethostent.o \
	mktemp.o strcasecmp.o
DEFS=	-DDEBUG
CFLAGS=	-O ${DEFS}

libresolv.a: ${OBJS}
	ar cru libresolv.a ${OBJS}

install: libresolv.a
	install -o bin -g bin -m 644 libresolv.a ${DESTDIR}/usr/lib
	ranlib ${DESTDIR}/usr/lib/libresolv.a

.c.o:
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

gethostnamadr.o: named/gethostnamadr.c
	${CC} -c ${CFLAGS} named/gethostnamadr.c
	-ld -x -r gethostnamadr.o
	mv a.out gethostnamadr.o

sethostent.o: named/sethostent.c
	${CC} -c ${CFLAGS} named/sethostent.c
	-ld -x -r sethostent.o
	mv a.out sethostent.o

clean: FRC
	rm -f *.o errs a.out core libresolv.a tags

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

herror.o: herror.c /usr/include/sys/types.h /usr/include/sys/uio.h
res_comp.o: res_comp.c /usr/include/sys/types.h /usr/include/stdio.h
res_comp.o: /usr/include/arpa/nameser.h
res_debug.o: res_debug.c /usr/include/sys/types.h /usr/include/netinet/in.h
res_debug.o: /usr/include/stdio.h /usr/include/arpa/nameser.h
res_init.o: res_init.c /usr/include/sys/types.h /usr/include/sys/socket.h
res_init.o: /usr/include/netinet/in.h /usr/include/stdio.h
res_init.o: /usr/include/arpa/nameser.h /usr/include/resolv.h
res_mkquery.o: res_mkquery.c /usr/include/stdio.h /usr/include/sys/types.h
res_mkquery.o: /usr/include/netinet/in.h /usr/include/arpa/nameser.h
res_mkquery.o: /usr/include/resolv.h
res_query.o: res_query.c /usr/include/sys/param.h /usr/include/sys/types.h
res_query.o: /usr/include/signal.h /usr/include/machine/trap.h
res_query.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
res_query.o: /usr/include/ctype.h /usr/include/netdb.h /usr/include/stdio.h
res_query.o: /usr/include/errno.h /usr/include/strings.h
res_query.o: /usr/include/arpa/inet.h /usr/include/arpa/nameser.h
res_query.o: /usr/include/resolv.h
res_send.o: res_send.c /usr/include/sys/param.h /usr/include/sys/types.h
res_send.o: /usr/include/signal.h /usr/include/machine/trap.h
res_send.o: /usr/include/sys/time.h /usr/include/time.h
res_send.o: /usr/include/sys/socket.h /usr/include/sys/uio.h
res_send.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/errno.h
res_send.o: /usr/include/arpa/nameser.h /usr/include/resolv.h
strcasecmp.o: strcasecmp.c
gethostnamadr.o: named/gethostnamadr.c /usr/include/sys/param.h
gethostnamadr.o: /usr/include/sys/types.h /usr/include/signal.h
gethostnamadr.o: /usr/include/machine/trap.h /usr/include/sys/socket.h
gethostnamadr.o: /usr/include/netinet/in.h /usr/include/ctype.h
gethostnamadr.o: /usr/include/netdb.h /usr/include/stdio.h /usr/include/errno.h
gethostnamadr.o: /usr/include/arpa/inet.h /usr/include/arpa/nameser.h
gethostnamadr.o: /usr/include/resolv.h
sethostent.o: named/sethostent.c /usr/include/sys/types.h
sethostent.o: /usr/include/arpa/nameser.h /usr/include/netinet/in.h
sethostent.o: /usr/include/resolv.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
