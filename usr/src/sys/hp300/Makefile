#
# Copyright (c) 1991 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh
#
#	@(#)Makefile	7.1 (Berkeley) %G%
#

COMM=	../net/*.[ch] ../netimp/*.[ch] ../netinet/*.[ch] ../netns/*.[ch] \
	../netiso/*.[ch] ../netccitt/*.[ch] \
	../kern/*.c ../ufs/*.[ch] ../nfs/*.[ch] ../vm/*.[ch] ../sys/*.h

# Makefile for hp300 tags file

all:
	@echo "make tags or links only"

THP300=	../hp300/tags
SHP300=	../hp300/hp300/*.[ch] ../hp300/include/*.h \
	../hp300/hpux/*.[ch] ../hp300/dev/*.[ch]
AHP300=	../hp300/hp300/*.s

# Directories in which to place hp300 tags links
DHP300=	hp300 dev hpux include

tags:
	-ctags -dtf ${THP300} ${COMM} ${SHP300}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AHP300} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${THP300}
	sort -o ${THP300} ${THP300}

links:
	-for i in ${DHP300}; do \
	    cd ../$$i && rm -f tags; ln -s ../tags tags; done
