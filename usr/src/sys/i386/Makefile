#	@(#)Makefile	7.6 (Berkeley) %G%

# Makefile for i386 tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

I386=	/sys/i386/conf/*.[ch] /sys/i386/eisa/*.[ch] /sys/i386/i386/*.[ch] \
	/sys/i386/include/*.[ch] /sys/i386/isa/*.[ch]
AI386=	/sys/i386/i386/*.s

tags::
	-ctags -dt ${COMM} ${I386}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AI386} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
