#	@(#)Makefile	7.6 (Berkeley) %G%

# Makefile for tahoe tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

TAHOE=	/sys/tahoe/conf/*.[ch] /sys/tahoe/if/*.[ch] \
	/sys/tahoe/include/*.[ch] /sys/tahoe/tahoe/*.[ch] \
	/sys/tahoe/vba/*.[ch]
ATAHOE=	/sys/tahoe/tahoe/*.s

tags::
	-ctags -dt ${COMM} ${TAHOE}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ATAHOE} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
