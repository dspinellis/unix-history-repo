#	@(#)Makefile	8.1 (Berkeley) 6/11/93

# Makefile for i386 links, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	conf eisa floppy i386 include isa mca scripts

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

I386=	/sys/i386/eisa/*.[ch] /sys/i386/i386/*.[ch] \
	/sys/i386/include/*.[ch] /sys/i386/isa/*.[ch]
AI386=	/sys/i386/i386/*.s

tags::
	-ctags -wdt ${COMM} ${I386}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${AI386} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
