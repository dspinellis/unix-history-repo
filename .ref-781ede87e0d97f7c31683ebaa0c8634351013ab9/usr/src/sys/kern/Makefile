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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	7.10 (Berkeley) %G%
#
# Put the ../sys stuff near the end so that subroutine definitions win when
# there is a struct tag with the same name (eg., vmmeter).  The real
# solution would probably be for ctags to generate "struct vmmeter" tags.

COMM=	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../netiso/*.[ch] ../netccitt/*.[ch] \
	../sys/*.h ../kern/*.c ../ufs/*.[ch] ../nfs/*.[ch] ../vm/*.[ch]
TVAX=	../vax/tags
SVAX=	../vax/if/*.[ch]  ../vax/mba/*.[ch] ../vax/uba/*.[ch] \
	../vax/bi/*.[ch] ../vax/vax/*.[ch] ../vax/include/*.h
AVAX=	../vax/vax/*.s

TTAHOE=	../tahoe/tags
STAHOE=	../tahoe/if/*.[ch]  ../tahoe/vba/*.[ch] ../tahoe/align/*.[ch] ../tahoe/math/*.h ../tahoe/tahoe/*.[ch] ../tahoe/include/*.h
ATAHOE=	../tahoe/tahoe/*.s ../tahoe/math/*.s

THP300=	../hp300/tags
SHP300=	../hp300/hpux/*.[ch] ../hp300/dev/*.[ch] ../hp300/hp300/*.[ch] \
	../hp300/include/*.h
AHP300=	../hp300/hp300/*.s

# Directories in which to place tags links (other than primary T$MACH)
DGEN=	kern sys net netimp netinet netns ufs
DVAX=	vax/if vax/mba vax/uba vax/bi vax/vax vax/include
DTAHOE=	tahoe/if tahoe/vba tahoe/align tahoe/math tahoe/include tahoe/tahoe
DHP300=	hp300/dev hp300/hpux hp300/hp300 hp300/include

all:
	@echo "make tags or links only"

tags: tahoetags vaxtags hp300tags

tahoetags:
	-ctags -dtf ${TTAHOE} ${COMM} ${STAHOE}
	egrep "^SCBVEC(.*)" ${ATAHOE} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${ATAHOE} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	sort -o ${TTAHOE} ${TTAHOE}

vaxtags:
	-ctags -dtf ${TVAX} ${COMM} ${SVAX}
	egrep "^SCBVEC(.*)" ${AVAX} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${AVAX} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	sort -o ${TVAX} ${TVAX}

hp300tags:
	-ctags -dtf ${THP300} ${COMM} ${SHP300}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AHP300} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${THP300}
	sort -o ${THP300} ${THP300}

links:
	-for i in ${DVAX}; do \
	    rm -f ../$$i/tags; ln -s ../tags ../$$i/tags; done
	-for i in ${DTAHOE}; do \
	    rm -f ../$$i/tags; ln -s ../tags ../$$i/tags; done
	-for i in ${DHP300}; do \
	    rm -f ../$$i/tags; ln -s ../tags ../$$i/tags; done
	-for i in ${DGEN}; do \
	    rm -f ../$$i/tags; \
	    ln -s ../machine/tags ../$$i/tags; done

init_sysent.c syscalls.c ../sys/syscall.h: makesyscalls.sh syscalls.master
	-mv -f init_sysent.c init_sysent.c.bak
	-mv -f syscalls.c syscalls.c.bak
	-mv -f ../sys/syscall.h ../sys/syscall.h.bak
	sh makesyscalls.sh syscalls.master
