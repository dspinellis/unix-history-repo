#	@(#)Makefile	7.2 (Berkeley) %G%

# Makefile for news3400 tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags only"

NEWS=	/sys/news3400/bm/*.[ch] /sys/news3400/conf/*.[ch] \
	/sys/news3400/fb/*.[ch] /sys/news3400/hbdev/*.[ch] \
	/sys/news3400/if/*.[ch] /sys/news3400/include/*.[ch] \
	/sys/news3400/iodev/*.[ch] /sys/news3400/iop/*.[ch] \
	/sys/news3400/news3400/*.[ch] /sys/news3400/sio/*.[ch]
ANEWS=	/sys/news3400/news3400/*.s

tags::
	-ctags -dt ${COMM} ${NEWS}
	egrep "^ENTRY(.*)|^ALTENTRY(.*)" ${ANEWS} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> tags
	sort -o tags tags
