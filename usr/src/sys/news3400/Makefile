#	@(#)Makefile	8.1 (Berkeley) 6/11/93

# Makefile for news3400 links, tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make links or tags only"

DIRS=	bm conf fb hbdev if include iodev iop news3400 newsos sio

links::
	-for i in ${DIRS}; do \
	    (cd $$i && { rm -f tags; ln -s ${SYSTAGS} tags; }) done

NEWS=	/sys/news3400/bm/*.[ch] /sys/news3400/fb/*.[ch] \
	/sys/news3400/hbdev/*.[ch] /sys/news3400/if/*.[ch] \
	/sys/news3400/include/*.[ch] /sys/news3400/iodev/*.[ch] \
	/sys/news3400/iop/*.[ch] /sys/news3400/news3400/*.[ch] \
	/sys/news3400/sio/*.[ch]
ANEWS=	/sys/news3400/news3400/*.s

tags::
	-ctags -wdt ${COMM} ${NEWS}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ANEWS} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
	chown bin.wsrc tags
	chmod 444 tags
