#	@(#)Makefile	7.1 (Berkeley) %G%

# Makefile for luna68k tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

ILUNA=	/sys/luna68k/conf/*.[ch] /sys/luna68k/dev/*.[ch] \
	/sys/luna68k/include/*.[ch] /sys/luna68k/luna68k/*.[ch]
ALUNA=	/sys/luna68k/luna68k/*.s

tags::
	-ctags -dt ${COMM} ${LUNA}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ALUNA} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
