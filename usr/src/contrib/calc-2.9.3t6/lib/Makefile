#
# lib - makefile for calc library scripts
#
# Copyright (c) 1994 David I. Bell and Landon Curt Noll
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
TOPDIR= /usr/local/lib
#TOPDIR= /usr/lib
#TOPDIR= /usr/libdata

LIBDIR= ${TOPDIR}/calc

# The calc files to install
#
CALC_FILES= README bigprime.cal deg.cal ellip.cal lucas.cal lucas_chk.cal \
	lucas_tbl.cal mersenne.cal mod.cal nextprim.cal pell.cal pi.cal \
	pollard.cal poly.cal psqrt.cal quat.cal regress.cal solve.cal \
	sumsq.cal surd.cal unitfrac.cal varargs.cal chrem.cal cryrand.cal \
	bindings altbind randmprime.cal test1000.cal

SHELL= /bin/sh

all: ${CALC_FILES} nextprime.cal

# link nextprime.cal to nextprim.cal
#
nextprime.cal: nextprim.cal
	rm -f nextprime.cal
	ln nextprim.cal nextprime.cal

clean:

clobber:
	rm -f nextprime.cal

install: all
	-@if [ ! -d ${TOPDIR} ]; then \
		echo mkdir ${TOPDIR}; \
		mkdir ${TOPDIR}; \
	fi
	-@if [ ! -d ${LIBDIR} ]; then \
		echo mkdir ${LIBDIR}; \
		mkdir ${LIBDIR}; \
	fi
	@for i in ${CALC_FILES}; do \
		echo rm -f ${LIBDIR}/$$i; \
		rm -f ${LIBDIR}/$$i; \
		echo cp $$i ${LIBDIR}; \
		cp $$i ${LIBDIR}; \
		echo chmod 0444 ${LIBDIR}/$$i; \
		chmod 0444 ${LIBDIR}/$$i; \
	done
	-rm -f ${LIBDIR}/nextprime.cal
	-ln ${LIBDIR}/nextprim.cal ${LIBDIR}/nextprime.cal 
