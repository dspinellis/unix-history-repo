#	@(#)Makefile	8.1 (Berkeley) 6/11/93

# Makefile for luna68k links, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	conf dev font include luna68k

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

ILUNA=	/sys/luna68k/dev/*.[ch] /sys/luna68k/include/*.[ch] \
	/sys/luna68k/luna68k/*.[ch]
ALUNA=	/sys/luna68k/luna68k/*.s

tags::
	-ctags -wdt ${COMM} ${LUNA}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ALUNA} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
