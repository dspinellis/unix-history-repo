#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.4 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
INCPATH=-I${SRCDIR}
LFLAGS=	
MACHINE= unknown
SRCDIR=	/sys/${MACHINE}/inline

# The program itself
#
PROG=	pc2

# Sources
#
RMTSRCS=${SRCDIR}/main.c ${SRCDIR}/machdep.c ${SRCDIR}/machpats.c \
	${SRCDIR}/libcpats.c
LOCSRCS=langpats.c

# Objects
#
RMTOBJS=main.o machdep.o machpats.o libcpats.o
LOCOBJS=langpats.o

# Header files
#
HDRS=	${SRCDIR}/inline.h

${PROG}: ${RMTOBJS} ${LOCOBJS} ${HDRS}
	cc ${LFLAGS} -o ${PROG} ${RMTOBJS} ${LOCOBJS}

${RMTOBJS}:
	cc ${CFLAGS} ${INCPATH} -c ${SRCDIR}/$*.c

${LOCOBJS}:
	cc ${CFLAGS} ${INCPATH} -c $*.c

install: ${PROG}
	install -s ${PROG} ${DESTDIR}/usr/lib/${PROG}

clean:
	rm -f a.out core ${RMTOBJS} ${LOCOBJS} ${PROG}

lint:
	lint ${LOCSRCS} ${RMTSRCS}

depend:
	cc -M ${INCPATH} ${LOCSRCS} ${RMTSRCS} | \
	sed 's/${MACHINE}/$${MACHINE}/' | \
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


langpats.o: langpats.c /sys/${MACHINE}/inline/inline.h
main.o: /sys/${MACHINE}/inline/main.c /usr/include/stdio.h /usr/include/ctype.h
main.o: /sys/${MACHINE}/inline/inline.h
machdep.o: /sys/${MACHINE}/inline/machdep.c /usr/include/stdio.h
machdep.o: /usr/include/ctype.h /sys/${MACHINE}/inline/inline.h
machpats.o: /sys/${MACHINE}/inline/machpats.c /sys/${MACHINE}/inline/inline.h
libcpats.o: /sys/${MACHINE}/inline/libcpats.c /sys/${MACHINE}/inline/inline.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
