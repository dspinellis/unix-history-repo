#
# Copyright (c) 1985 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) 5/13/86
#
DESTDIR=
HELP=   ../man/nslookup.help
DEFS=	
CFLAGS=	-g ${DEFS}
LIBS=	-ll -lresolv
LFLAGS=	

# The name server assistant
#
PROG=	nslookup

# Sources
#
SRCS=	main.c getinfo.c debug.c send.c skip.c list.c subr.c 

# Objects
#
OBJS=	main.o getinfo.o debug.o send.o skip.o list.o subr.o lex.yy.o 

# Header files
#
HDRS=	res.h

${PROG}: ${OBJS} ${HDRS}
	cc ${LFLAGS} -o ${PROG} ${OBJS} ${LIBS}

${OBJS}:
	cc ${CFLAGS} ${INCPATH} -c $*.c

lex.yy.c: commands.l
	${LEX} commands.l

install: ${PROG} ${HELP}
	install -c ${PROG} ${DESTDIR}/usr/local/${PROG}
	install -c ${HELP} ${DESTDIR}/usr/local/${HELP}

clean:
	rm -f a.out core tags errs log lex.yy.c ${OBJS} ${PROG}

lint:
	lint -hn ${INCPATH} ${DEFS} ${SRCS} 

depend:
	cc -M ${INCPATH} ${SRCS} | \
	awk ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$2 } } \
	      END { print rec } ' > makedep
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it


main.o: main.c /usr/include/stdio.h /usr/include/strings.h
main.o: /usr/include/sys/types.h /usr/include/netdb.h /usr/include/sys/socket.h
main.o: /usr/include/netinet/in.h /usr/include/arpa/nameser.h
main.o: /usr/include/resolv.h /usr/include/signal.h /usr/include/setjmp.h
main.o: ./res.h
getinfo.o: getinfo.c /usr/include/sys/types.h /usr/include/sys/socket.h
getinfo.o: /usr/include/netinet/in.h /usr/include/stdio.h /usr/include/ctype.h
getinfo.o: /usr/include/arpa/nameser.h /usr/include/resolv.h ./res.h
debug.o: debug.c /usr/include/sys/types.h /usr/include/netinet/in.h
debug.o: /usr/include/stdio.h /usr/include/arpa/nameser.h /usr/include/resolv.h
debug.o: ./res.h
send.o: send.c /usr/include/sys/types.h /usr/include/sys/time.h
send.o: /usr/include/time.h /usr/include/sys/socket.h /usr/include/netinet/in.h
send.o: /usr/include/stdio.h /usr/include/arpa/nameser.h /usr/include/resolv.h
send.o: ./res.h
skip.o: skip.c /usr/include/sys/types.h /usr/include/netinet/in.h
skip.o: /usr/include/stdio.h /usr/include/arpa/nameser.h
list.o: list.c /usr/include/sys/types.h /usr/include/sys/socket.h
list.o: /usr/include/netinet/in.h /usr/include/netdb.h /usr/include/stdio.h
list.o: /usr/include/strings.h /usr/include/ctype.h /usr/include/arpa/nameser.h
list.o: /usr/include/resolv.h ./res.h
subr.o: subr.c /usr/include/stdio.h /usr/include/strings.h
subr.o: /usr/include/sys/types.h /usr/include/netdb.h /usr/include/sys/socket.h
subr.o: /usr/include/netinet/in.h /usr/include/arpa/nameser.h
subr.o: /usr/include/signal.h /usr/include/setjmp.h ./res.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
