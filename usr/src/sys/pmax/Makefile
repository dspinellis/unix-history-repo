#	@(#)Makefile	7.1 (Berkeley) %G%

# Makefile for pmax tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags or links only"

TPMAX=	../pmax/tags
SPMAX=	../pmax/pmax/*.[ch] ../pmax/include/*.h ../pmax/dev/*.[ch]
APMAX=	../pmax/pmax/*.s

# Directories in which to place pmax tags links
DPMAX=	pmax dev include

tags: FRC
	-ctags -dtf ${TPMAX} ${COMM} ${SPMAX}
	egrep "^LEAF(.*)|^ALEAF(.*)|^NON_LEAF(.*)" ${APMAX} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TPMAX}
	sort -o ${TPMAX} ${TPMAX}

links:
	-for i in ${DPMAX}; do \
	    cd ../$$i && rm -f tags; ln -s ../tags tags; done

FRC:
