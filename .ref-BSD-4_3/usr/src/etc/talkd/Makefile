#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) 3/13/86
#
DESTDIR=
OBJS=	talkd.o announce.o process.o table.o print.o
SRCS=	talkd.c announce.c process.c table.c print.c
CFLAGS= -O

all:	talkd

talkd: ${OBJS}
	cc ${CFLAGS} -o talkd ${OBJS}

install: talkd
	install -s talkd ${DESTDIR}/etc/ntalkd

clean:
	rm -f ${OBJS} errs core a.out talkd

tags:	${SRCS}
	ctags ${SOURCE}

depend: ${SRCS}
	for i in ${SRCS}; do \
	    cc -M $$i | sed 's/\.o//' | \
	    awk ' { if ($$1 != prev) \
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

talkd: talkd.c /usr/include/stdio.h /usr/include/errno.h /usr/include/signal.h
talkd: /usr/include/syslog.h /usr/include/protocols/talkd.h
talkd: /usr/include/sys/types.h /usr/include/sys/socket.h
announce: announce.c /usr/include/sys/types.h /usr/include/sys/stat.h
announce: /usr/include/sgtty.h /usr/include/sys/ioctl.h
announce: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
announce: /usr/include/sys/ioctl.h /usr/include/sys/time.h /usr/include/time.h
announce: /usr/include/stdio.h /usr/include/sys/wait.h /usr/include/errno.h
announce: /usr/include/syslog.h /usr/include/protocols/talkd.h
announce: /usr/include/sys/types.h /usr/include/sys/socket.h
process: process.c /usr/include/sys/types.h /usr/include/sys/stat.h
process: /usr/include/stdio.h /usr/include/syslog.h /usr/include/netdb.h
process: /usr/include/netinet/in.h /usr/include/protocols/talkd.h
process: /usr/include/sys/types.h /usr/include/sys/socket.h /usr/include/utmp.h
table: table.c /usr/include/stdio.h /usr/include/sys/time.h /usr/include/time.h
table: /usr/include/syslog.h /usr/include/protocols/talkd.h
table: /usr/include/sys/types.h /usr/include/sys/socket.h
print: print.c /usr/include/stdio.h /usr/include/syslog.h
print: /usr/include/protocols/talkd.h /usr/include/sys/types.h
print: /usr/include/sys/socket.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
