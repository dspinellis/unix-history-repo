#	@(#)Makefile	7.2 (Berkeley) %G%

# Makefile for sparc tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

SPARC=	/sys/sparc/dev/*.[ch] /sys/sparc/fpu/*.[ch] \
	/sys/sparc/include/*.[ch] /sys/sparc/rcons/*.[ch] \
	/sys/sparc/sbus/*.[ch] /sys/sparc/sparc/*.[ch] \
	/sys/sparc/sunos/*.[ch]
ASPARC=	/sys/sparc/sparc/*.s

tags::
	-ctags -dt ${COMM} ${SPARC}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ASPARC} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
