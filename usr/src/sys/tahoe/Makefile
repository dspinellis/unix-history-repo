#	@(#)Makefile	7.3 (Berkeley) %G%

# Makefile for tahoe tags file

all:
	@echo "make tags or links only"

TTAHOE=	../tahoe/tags
STAHOE=	../tahoe/tahoe/*.[ch] ../tahoe/include/*.h ../tahoe/if/*.[ch] \
	../tahoe/vba/*.[ch] ../tahoe/align/*.[ch] ../tahoe/math/*.h
ATAHOE=	../tahoe/tahoe/*.s ../tahoe/math/*.s


# Directories in which to place tahoe tags links
DTAHOE=	if vba align math include

tags:
	-ctags -dtf ${TTAHOE} ${COMM} ${STAHOE}
	egrep "^SCBVEC(.*)" ${ATAHOE} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${ATAHOE} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TTAHOE}
	sort -o ${TTAHOE} ${TTAHOE}

links:
	-for i in ${DTAHOE}; do \
	    cd $$i && rm -f tags; ln -s ../tags tags; done
