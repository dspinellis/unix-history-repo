#	@(#)Makefile	7.6 (Berkeley) %G%

# Makefile for hp300 tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

HP300=	/sys/hp/dev/*.[ch] /sys/hp/hpux/*.[ch] /sys/hp300/conf/*.[ch] \
	/sys/hp300/dev/*.[ch] /sys/hp300/hp300/*.[ch] /sys/hp300/include/*.[ch]
AHP300=	/sys/hp300/hp300/*.s

tags::
	-ctags -dt ${COMM} ${HP300}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AHP300} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
