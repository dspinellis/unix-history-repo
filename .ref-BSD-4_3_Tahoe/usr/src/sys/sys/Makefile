#
# Copyright (c) 1986 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	7.5 (Berkeley) 3/28/87
#
# Put the ../h stuff near the end so that subroutine definitions win when
# there is a struct tag with the same name (eg., vmmeter).  The real
# solution would probably be for ctags to generate "struct vmmeter" tags.

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
	ctags -dtf ${TVAX} ${SVAX}
	egrep "^SCBVEC(.*)" ${AVAX} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${AVAX} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	sort -o ${TVAX} ${TVAX}
	rm -f ${TVAX}.old
	-mv ${TTAHOE} ${TTAHOE}.old
	ctags -dtf ${TTAHOE} ${STAHOE}
	egrep "^SCBVEC(.*)" ${ATAHOE} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${ATAHOE} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	sort -o ${TTAHOE} ${TTAHOE}
	rm -f ${TTAHOE}.old

links:
	-for i in ${DVAX}; do rm -f ../$$i/tags; \
	ln -s ../vax/tags ../$$i/tags; done
	-for i in ${DTAHOE}; do rm -f ../$$i/tags; \
	ln -s ../tahoe/tags ../$$i/tags; done
	-for i in ${DGEN}; do rm -f ../$$i/tags; \
	ln -s ../machine/tags ../$$i/tags; done

FRC:
