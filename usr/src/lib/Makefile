#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.10 (Berkeley) %G%
#
DESTDIR=
CFLAGS=	-O
TAGSFILE=tags

# Subdirectories whose routines are included in the making of the
# master tags file (/usr/lib/tags); the Fortran libraries should
# be on this list, but we don't control them....
#
TAGSDIR=libcurses libdbm libln libm libmp libpc libtermlib

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	learn lib2648 libF77 libI77 libU77 libcurses libdbm libg libln \
	libom libm libmp libpc libplot libtermlib liby lpr me sendmail

# Shell scripts that need only be installed and are never removed.
#
SCRIPT=	makewhatis

# C programs that live in the current directory and do not need
# explicit make lines.
#
STD=	getNAME makekey

# C programs that live in the current directory and need explicit make lines.
#
NSTD=	

all:	${SUBDIR} ${STD} ${NSTD}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

FRC:

${STD}:
	cc ${CFLAGS} -o $@ $@.c

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done
	-for i in ${SCRIPT}; do (install -c $$i.sh ${DESTDIR}/usr/lib/$$i); done
	-for i in ${STD} ${NSTD}; do (install -s $$i ${DESTDIR}/usr/lib/$$i); done
	install -c lib.b ${DESTDIR}/usr/lib/lib.b

tags:
	-for i in ${TAGSDIR}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	done

clean:
	rm -f a.out core *.s *.o tags
	-for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f ${STD} ${NSTD}

depend:
	for i in ${STD} ${NSTD}; do \
	    cc -M ${INCPATH} $$i.c | sed 's/\.o//' | \
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

# Files listed in ${NSTD} have explicit make lines given below.

# DO NOT DELETE THIS LINE -- make depend uses it

getNAME: getNAME.c /usr/include/strings.h /usr/include/stdio.h
makekey: makekey.c
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
