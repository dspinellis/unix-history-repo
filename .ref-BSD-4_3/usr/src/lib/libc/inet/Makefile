#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 9/5/85
#

SRCS=	inet_addr.c inet_network.c \
	inet_netof.c inet_ntoa.c inet_lnaof.c \
	inet_makeaddr.c
OBJS=	inet_addr.o inet_network.o \
	inet_netof.o inet_ntoa.o inet_lnaof.o \
	inet_makeaddr.o
CFLAGS=	-O ${DEFS}
TAGSFILE=tags

.c.o:
	${CC} -p -c ${CFLAGS} $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

inetlib inetlib_p: ${OBJS}
	@echo "building profiled inetlib"
	@cd profiled; ar cru ../inetlib_p ${OBJS}
	@echo "building normal inetlib"
	@ar cru inetlib ${OBJS}

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f inetlib inetlib_p *.o profiled/*.o errs a.out core \
		tags Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
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

inet_addr.o: inet_addr.c /usr/include/sys/types.h /usr/include/ctype.h
inet_addr.o: /usr/include/netinet/in.h
inet_network.o: inet_network.c /usr/include/sys/types.h /usr/include/ctype.h
inet_netof.o: inet_netof.c /usr/include/sys/types.h /usr/include/netinet/in.h
inet_ntoa.o: inet_ntoa.c /usr/include/sys/types.h /usr/include/netinet/in.h
inet_lnaof.o: inet_lnaof.c /usr/include/sys/types.h /usr/include/netinet/in.h
inet_makeaddr.o: inet_makeaddr.c /usr/include/sys/types.h
inet_makeaddr.o: /usr/include/netinet/in.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
