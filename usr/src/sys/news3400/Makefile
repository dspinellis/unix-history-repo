#	@(#)Makefile	7.1 (Berkeley) %G%

# Makefile for news3400 tags file

.include "../kern/Make.tags.inc"

all:
	@echo "make tags or links only"

TNEWS=	../news3400/tags
SNEWS=	../news3400/news3400/*.[ch] \
	../news3400/include/*.h \
	../news3400/bm/*.[ch] \
	../news3400/fb/*.[ch] \
	../news3400/hbdev/*.[ch] \
	../news3400/if/*.[ch] \
	../news3400/iodev/*.[ch] \
	../news3400/iop/*.[ch] \
	../news3400/sio/*.[ch]
ANEWS=	../news3400/news3400/*.s

# Directories in which to place news3400 tags links
DNEWS=	news3400 include

tags: FRC
	-ctags -tf ${TNEWS} ${COMM} ${SNEWS}
	egrep "^LEAF(.*)|^ALEAF(.*)|^NON_LEAF(.*)" ${ANEWS} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TNEWS}
	sort -o ${TNEWS} ${TNEWS}

links:
	-for i in ${DNEWS}; do \
	    cd ../$$i && rm -f tags; ln -s ../tags tags; done

FRC:
