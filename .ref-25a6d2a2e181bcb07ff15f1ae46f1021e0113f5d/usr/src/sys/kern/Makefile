#
# Copyright (c) 1986 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	7.6 (Berkeley) %G%
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
