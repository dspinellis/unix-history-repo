#	@(#)Makefile	8.1 (Berkeley) 6/11/93

# Makefile for sparc links, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	conf dev fpu include rcons sbus sparc sunos

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

SPARC=	/sys/sparc/dev/*.[ch] /sys/sparc/fpu/*.[ch] \
	/sys/sparc/include/*.[ch] /sys/sparc/rcons/*.[ch] \
	/sys/sparc/sbus/*.[ch] /sys/sparc/sparc/*.[ch] \
	/sys/sparc/sunos/*.[ch]
ASPARC=	/sys/sparc/sparc/*.s

tags::
	-ctags -wdt ${COMM} ${SPARC}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ASPARC} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
