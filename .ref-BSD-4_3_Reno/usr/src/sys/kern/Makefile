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
#	@(#)Makefile	7.8 (Berkeley) 6/21/90
#
# Put the ../sys stuff near the end so that subroutine definitions win when
# there is a struct tag with the same name (eg., vmmeter).  The real
# solution would probably be for ctags to generate "struct vmmeter" tags.

COMM=	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../kern/*.[ch] ../ufs/*.[ch] ../nfs/*.[ch]
HDR=	../sys/*.h
TVAX=	../vax/tags
SVAX=	../vaxif/*.[ch]  ../vaxmba/*.[ch] ../vaxuba/*.[ch] ../vax/*.[ch]
AVAX=	../vax/*.s

TTAHOE=	../tahoe/tags
STAHOE=	../tahoeif/*.[ch]  ../tahoevba/*.[ch] ../tahoe/*.[ch]
ATAHOE=	../tahoe/*.s

THP300=	../hp300/tags
SHP300=	 ../hpux/*.[ch]../hpdev/*.[ch] ../hp300/*.[ch]
AHP300=	../hp300/*.s

# Directories in which to place tags links (other than primary T$MACH)
DGEN=	kern sys net netimp netinet netns ufs
DVAX=	vaxif vaxmba vaxuba
DTAHOE=	tahoeif tahoevba
DHP300=	hpdev hpux

all:
	@echo "make tags or links only"

tags: tahoetags vaxtags hp300tags

tahoetags:
	-ctags -dtf ${TTAHOE} ${COMM} ${STAHOE} ${HDR}
	egrep "^SCBVEC(.*)" ${ATAHOE} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${ATAHOE} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	sort -o ${TTAHOE} ${TTAHOE}

vaxtags:
	-ctags -dtf ${TVAX} ${COMM} ${SVAX} ${HDR}
	egrep "^SCBVEC(.*)" ${AVAX} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${AVAX} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	sort -o ${TVAX} ${TVAX}

hp300tags:
	-ctags -dtf ${THP300} ${COMM} ${SHP300} ${HDR}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AHP300} | \
sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${THP300}
	sort -o ${THP300} ${THP300}

links:
	-for i in ${DVAX}; do \
	    rm -f ../$$i/tags; ln -s ../vax/tags ../$$i/tags; done
	-for i in ${DTAHOE}; do \
	    rm -f ../$$i/tags; ln -s ../tahoe/tags ../$$i/tags; done
	-for i in ${DHP300}; do \
	    rm -f ../$$i/tags; ln -s ../tahoe/tags ../$$i/tags; done
	-for i in ${DGEN}; do \
	    rm -f ../$$i/tags; \
	    ln -s ../machine/tags ../$$i/tags; done

init_sysent.c syscalls.c ../sys/syscall.h: makesyscalls.sh syscalls.master
	-mv -f init_sysent.c init_sysent.c.bak
	-mv -f syscalls.c syscalls.c.bak
	-mv -f ../sys/syscall.h ../sys/syscall.h.bak
	sh makesyscalls.sh syscalls.master
