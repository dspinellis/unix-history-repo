#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#
#       Makefile for at, atq, atrm, and atrun
#
ALL=		atq atrm at atrun
SRCS=		atq.c atrm.c at.c atrun.c
CFLAGS=		-O
INCPATH=
DESTDIR=
ATRUNDIR=	/usr/lib
SPOOLDIRS=	/usr/spool/at /usr/spool/at/past
SPOOLFILES=	/usr/spool/at/lasttimedone

all: ${ALL}

at: at.c
	$(CC) $(CFLAGS) -o at at.c

atq: atq.c
	$(CC) $(CFLAGS) -o atq atq.c

atrm: atrm.c
	$(CC) $(CFLAGS) -o atrm atrm.c

atrun: atrun.c
	$(CC) $(CFLAGS) -o atrun atrun.c

install: all
	-mkdir $(SPOOLDIRS)
	chown daemon $(SPOOLDIRS)
	chgrp daemon $(SPOOLDIRS)
	chmod 755    $(SPOOLDIRS)
	chown daemon $(SPOOLFILES)
	chgrp daemon $(SPOOLFILES)
	install -s atrun $(ATRUNDIR)
	install -s atq $(DESTDIR)/usr/bin
	install -s -o daemon -m 4755 at $(DESTDIR)/usr/bin
	install -s -o daemon -m 4755 atrm $(DESTDIR)/usr/bin

clean:
	rm -f $(ALL) errs *.o

print:
	lpr ${SRCS}

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


atq.o: atq.c /usr/include/stdio.h /usr/include/sys/types.h
atq.o: /usr/include/sys/file.h /usr/include/sys/dir.h /usr/include/sys/stat.h
atq.o: /usr/include/sys/time.h /usr/include/pwd.h /usr/include/ctype.h
atrm.o: atrm.c /usr/include/stdio.h /usr/include/pwd.h /usr/include/ctype.h
atrm.o: /usr/include/sys/types.h /usr/include/sys/dir.h /usr/include/sys/file.h
atrm.o: /usr/include/sys/stat.h
at.o: at.c /usr/include/stdio.h /usr/include/ctype.h /usr/include/signal.h
at.o: /usr/include/sys/time.h /usr/include/sys/file.h
atrun.o: atrun.c /usr/include/stdio.h /usr/include/sys/types.h
atrun.o: /usr/include/sys/dir.h /usr/include/sys/file.h /usr/include/sys/time.h
atrun.o: /usr/include/sys/stat.h /usr/include/pwd.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
