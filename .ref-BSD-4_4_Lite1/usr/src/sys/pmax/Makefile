#	@(#)Makefile	8.1 (Berkeley) 6/16/93

# Makefile for pmax links, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	conf dev dist include pmax ultrix

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

PMAX=	/sys/pmax/dev/*.[ch] /sys/pmax/include/*.[ch] \
	/sys/pmax/pmax/*.[ch] /sys/pmax/ultrix/*.[ch]
APMAX=	/sys/pmax/pmax/*.s

tags::
	-ctags -wdt ${COMM} ${PMAX}
	egrep "^LEAF(.*)|^[AN]LEAF(.*)|^NON_LEAF(.*)" ${APMAX} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
