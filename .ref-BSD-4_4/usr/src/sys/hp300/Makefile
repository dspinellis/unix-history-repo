#	@(#)Makefile	8.1 (Berkeley) 6/11/93

# Makefile for hp300 links, tags files

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	conf dev hp300 include scripts

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

HP300=	/sys/hp/dev/*.[ch] /sys/hp/hpux/*.[ch] /sys/hp300/dev/*.[ch] \
	/sys/hp300/hp300/*.[ch] /sys/hp300/include/*.[ch]
AHP300=	/sys/hp300/hp300/*.s

tags::
	-ctags -wdt ${COMM} ${HP300}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AHP300} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
