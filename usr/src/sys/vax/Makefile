#	@(#)Makefile	7.7 (Berkeley) %G%

# Makefile for vax link, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	bi cassette conf consolerl datakit dist floppy if include mba \
	mdec uba vax

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

VAX=	/sys/vax/bi/*.[ch] /sys/vax/if/*.[ch] /sys/vax/include/*.[ch] \
	/sys/vax/mba/*.[ch] /sys/vax/uba/*.[ch] /sys/vax/vax/*.[ch]
AVAX=	/sys/vax/vax/*.s

tags::
	-ctags -wdt ${COMM} ${VAX}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AVAX} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
