#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.5 (Berkeley) 9/5/85
#
# Machine dependent routines for the VAX are located here
#
COMPAT=	compat-4.1
ALL=	gen net stdio sys ${COMPAT}
TAGSFILE=tags

vaxlib vaxlib_p: ${ALL}
	-mkdir tmp
	for i in ${ALL}; do (cd tmp; ar x ../$$i/$${i}lib); done
	ar cr vaxlib `ls tmp/*.o | sort -t/ +1`
	rm -f tmp/*
	for i in ${ALL}; do (cd tmp; ar x ../$$i/$${i}lib_p); done
	ar cr vaxlib_p `ls tmp/*.o | sort -t/ +1`
	rm -rf tmp

${ALL}:	FRC
	cd $@; make ${MFLAGS} DEFS=${DEFS}

FRC:

tags:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} TAGSFILE=../${TAGSFILE} tags); \
	done

clean:
	for i in ${ALL}; do (cd $$i; make ${MFLAGS} clean); done
	rm -f vaxlib vaxlib_p tmp/*.o;
	-rmdir tmp

depend:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} DEFS=${DEFS} depend); done
