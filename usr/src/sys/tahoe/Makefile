#	@(#)Makefile	8.1 (Berkeley) 6/11/93

# Makefile for tahoe links, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	align conf dist if include math tahoe vba

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

TAHOE=	/sys/tahoe/if/*.[ch] /sys/tahoe/include/*.[ch] \
	/sys/tahoe/tahoe/*.[ch] /sys/tahoe/vba/*.[ch]
ATAHOE=	/sys/tahoe/tahoe/*.s

tags::
	-ctags -wdt ${COMM} ${TAHOE}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ATAHOE} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
