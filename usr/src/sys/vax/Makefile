#	@(#)Makefile	7.2 (Berkeley) %G%

# Makefile for vax tags file

all:
	@echo "make tags or links only"

TVAX=	../vax/tags
SVAX=	../vax/if/*.[ch]  ../vax/mba/*.[ch] ../vax/uba/*.[ch] \
	../vax/bi/*.[ch] ../vax/vax/*.[ch] ../vax/include/*.h
AVAX=	../vax/vax/*.s

# Directories in which to place vax tags links
DVAX=	if mba uba bi vax include

tags:
	-ctags -dtf ${TVAX} ${COMM} ${SVAX}
	egrep "^SCBVEC(.*)" ${AVAX} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);X\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	egrep "^ENTRY(.*)|^JSBENTRY(.*)" ${AVAX} | \
	    sed "s;\([^:]*\):\([^(]*\)(\([^, )]*\)\(.*\);\3 \1 /^\2(\3\4$$/;" \
		>> ${TVAX}
	sort -o ${TVAX} ${TVAX}

links:
	-for i in ${DVAX}; do \
	    cd $$i && rm -f tags; ln -s ../tags tags; done
