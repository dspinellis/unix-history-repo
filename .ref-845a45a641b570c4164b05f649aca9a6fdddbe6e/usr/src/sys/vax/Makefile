#	@(#)Makefile	7.6 (Berkeley) %G%

# Makefile for vax tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

VAX=	/sys/vax/bi/*.[ch] /sys/vax/conf/*.[ch] /sys/vax/if/*.[ch] \
	/sys/vax/include/*.[ch] /sys/vax/mba/*.[ch] /sys/vax/uba/*.[ch] \
	/sys/vax/vax/*.[ch]
AVAX=	/sys/vax/vax/*.s

tags::
	-ctags -dt ${COMM} ${VAX}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AVAX} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
