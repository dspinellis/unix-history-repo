#
# lib - makefile for calc library scripts
#
# Copyright (c) 1993 David I. Bell and Landon Curt Noll
# Permission is granted to use, distribute, or modify this source,
# provided that this copyright notice remains intact.
#
# Arbitrary precision calculator.
#
# calculator by David I. Bell
# makefile by Landon Curt Noll

# Normally, the upper level makefile will set these values.  We provide
# a default here just in case you want to build from this directory.
#
# where to install things
LIBDIR= /usr/local/lib/calc
# how to build a directory
MKDIR=mkdir -p
#MKDIR=mkdir

# The calc files to install
#
CALC_FILES= README bigprime.cal deg.cal ellip.cal lucas.cal lucas_chk.cal \
	lucas_tbl.cal mersenne.cal mod.cal nextprim.cal pell.cal pi.cal \
	pollard.cal poly.cal psqrt.cal quat.cal regress.cal solve.cal \
	sumsq.cal surd.cal unitfrac.cal varargs.cal chrem.cal cryrand.cal \
	bindings

SHELL= /bin/sh

all: ${CALC_FILES}

clean:

clobber:

install: all
	-@if [ ! -d ${LIBDIR} ]; then \
		echo ${MKDIR} ${LIBDIR}; \
		${MKDIR} ${LIBDIR}; \
	fi
	@for i in ${CALC_FILES}; do \
		echo rm -f ${LIBDIR}/$$i; \
		rm -f ${LIBDIR}/$$i; \
		echo chmod 0444 $$i; \
		chmod 0444 $$i; \
		echo cp $$i ${LIBDIR}; \
		cp $$i ${LIBDIR}; \
	done
