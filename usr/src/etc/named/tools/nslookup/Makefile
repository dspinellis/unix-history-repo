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
#	@(#)Makefile	5.16 (Berkeley) 2/20/88
#
CFLAGS=	-O
RES=
LIBC=	/lib/libc.a
LIBS=	-ll ${RES}
CSRCS=	main.c getinfo.c debug.c send.c skip.c list.c subr.c
SRCS=	${CSRCS} commands.c
OBJS=	main.o getinfo.o debug.o send.o skip.o list.o subr.o commands.o

.SUFFIXES: .1 .0

.1.0:
	${DESTDIR}/usr/man/manroff $*.1 > $*.0

nslookup: ${OBJS} ${LIBC}
	${CC} -o $@ ${OBJS} ${LIBS}

clean: FRC
	rm -f ${OBJS} core nslookup commands.c nslookup.0 lex.yy.c lex.yy.o

depend: ${SRCS} FRC
	mkdep ${CFLAGS} ${SRCS}

install: nslookup.0 FRC
	install -s -o bin -g bin -m 755 nslookup ${DESTDIR}/usr/local/nslookup
	install -c -o bin -g bin -m 444 nslookup.help ${DESTDIR}/usr/local/nslookup.help
	install -c -o bin -g bin -m 444 nslookup.0 ${DESTDIR}/usr/local/man/cat1/nslookup.0

lint: ${SRCS} FRC
	lint ${CFLAGS} ${SRCS}

tags: ${CSRCS} FRC
	ctags ${CSRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

main.o: main.c /usr/include/stdio.h /usr/include/strings.h
main.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
main.o: /usr/include/machine/trap.h /usr/include/netdb.h
main.o: /usr/include/sys/socket.h /usr/include/netinet/in.h
main.o: /usr/include/arpa/nameser.h /usr/include/resolv.h /usr/include/signal.h
main.o: /usr/include/setjmp.h res.h
getinfo.o: getinfo.c /usr/include/sys/types.h /usr/include/sys/socket.h
getinfo.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/ctype.h
getinfo.o: /usr/include/arpa/nameser.h /usr/include/resolv.h res.h
debug.o: debug.c /usr/include/sys/types.h /usr/include/netinet/in.h
debug.o: /usr/include/stdio.h /usr/include/arpa/nameser.h /usr/include/resolv.h
debug.o: res.h
send.o: send.c /usr/include/sys/types.h /usr/include/sys/time.h
send.o: /usr/include/time.h /usr/include/sys/socket.h /usr/include/netinet/in.h
send.o: /usr/include/stdio.h /usr/include/arpa/nameser.h /usr/include/resolv.h
send.o: res.h
skip.o: skip.c /usr/include/sys/types.h /usr/include/netinet/in.h
skip.o: /usr/include/stdio.h /usr/include/arpa/nameser.h
list.o: list.c /usr/include/sys/types.h /usr/include/sys/socket.h
list.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/stdio.h
list.o: /usr/include/strings.h /usr/include/ctype.h /usr/include/arpa/nameser.h
list.o: /usr/include/resolv.h res.h
subr.o: subr.c /usr/include/stdio.h /usr/include/strings.h
subr.o: /usr/include/sys/types.h /usr/include/netdb.h /usr/include/sys/socket.h
subr.o: /usr/include/netinet/in.h /usr/include/arpa/nameser.h
subr.o: /usr/include/signal.h /usr/include/machine/trap.h /usr/include/setjmp.h
subr.o: res.h
commands.o: commands.c /usr/include/stdio.h res.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
