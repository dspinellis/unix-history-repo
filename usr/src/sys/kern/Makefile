#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.4 (Berkeley) %G%
#
# Put the ../h stuff near the end so that subroutine definitions win when
# there is a struct tag with the same name (eg., vmmeter).  The real
# solution would probably be for ctags to generate "struct vmmeter" tags.
CTAGS=	/usr/ucb/ctags

TVAX=	../vax/tags
SVAX=	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../sys/*.[ch] ../vaxif/*.[ch]  ../vaxmba/*.[ch] ../vaxuba/*.[ch] \
	../vax/*.[ch] ../h/*.h
AVAX=	../vax/*.s

TTAHOE=	../tahoe/tags
STAHOE=	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../sys/*.[ch] ../tahoeif/*.[ch]  ../tahoevba/*.[ch] ../tahoe/*.[ch] \
	../h/*.h
ATAHOE=	../tahoe/*.s

DGEN=	h sys net netimp netinet netns
DVAX=	vaxif vaxmba vaxuba
DTAHOE=	tahoeif tahoevba

all:
	@echo "make tags or links only"

tags: FRC
	-mv ${TVAX} ${TVAX}.old
	${CTAGS} -dtf ${TVAX} ${SVAX}
	/usr/bin/egrep "^SCBVEC(.*)" ${AVAX} | \
/bin/sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	/usr/bin/egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${AVAX} | \
/bin/sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	/usr/bin/sort -o ${TVAX} ${TVAX}
	rm -f ${TVAX}.old
	-mv ${TTAHOE} ${TTAHOE}.old
	${CTAGS} -dtf ${TTAHOE} ${STAHOE}
	/usr/bin/egrep "^SCBVEC(.*)" ${ATAHOE} | \
/bin/sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	/usr/bin/egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${ATAHOE} | \
/bin/sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	/usr/bin/sort -o ${TTAHOE} ${TTAHOE}
	rm -f ${TTAHOE}.old

links:
	-for i in ${DVAX}; do rm -f ../$$i/tags; \
	ln -s ../vax/tags ../$$i/tags; done
	-for i in ${DTAHOE}; do rm -f ../$$i/tags; \
	ln -s ../tahoe/tags ../$$i/tags; done
	-for i in ${DGEN}; do rm -f ../$$i/tags; \
	ln -s ../machine/tags ../$$i/tags; done

FRC:
