#
# help - makefile for calc help files
#
# Copyright (c) 1994 David I. Bell and Landon Curt Noll
# Permission is granted to use, distribute, or modify this source,
# provided that this copyright notice remains intact.
#
# Arbitrary precision calculator.
#
# calculator by David I. Bell
# makefile by Landon Curt Noll

SHELL= /bin/sh

# Normally, the upper level makefile will set these values.  We provide
# a default here just in case you want to build from this directory.
#
TOPDIR= /usr/local/lib
#TOPDIR= /usr/lib
#TOPDIR= /usr/libdata

LIBDIR= ${TOPDIR}/calc
HELPDIR= ${LIBDIR}/help

# standard tools
SED= sed

# Standard help files
#
# The obj.file is special and is not listed here.
#
STD_HELP_FILES1= intro overview help assoc builtin command config \
	define environment expression file history interrupt list mat
STD_HELP_FILES2= operator statement types usage variable
STD_HELP_FILES3= todo credit
STD_HELP_FILES= ${STD_HELP_FILES1} ${STD_HELP_FILES2} ${STD_HELP_FILES3}

# Help files that are constructed from other sources
#
# The obj.file is special and is not listed here.
#
BUILT_HELP_FILES= bindings altbind changes libcalc stdlib bugs

all: ${STD_HELP_FILES} obj.file ${BUILT_HELP_FILES} full

bindings: ../lib/bindings
	rm -f bindings
	cp ../lib/bindings bindings
	chmod 0444 bindings

altbind: ../lib/altbind
	rm -f altbind
	cp ../lib/altbind altbind
	chmod 0444 altbind

stdlib: ../lib/README
	rm -f stdlib
	cp ../lib/README stdlib
	chmod 0444 stdlib

changes: ../CHANGES
	rm -f changes
	cp ../CHANGES changes
	chmod 0444 changes

libcalc: ../LIBRARY
	rm -f libcalc
	${SED} -e 's:$${LIBDIR}:${LIBDIR}:g' < ../LIBRARY > libcalc
	chmod 0444 libcalc

bugs: ../BUGS
	rm -f bugs
	cp ../BUGS bugs
	chmod 0444 bugs

full: ${STD_HELP_FILES} ${BUILT_HELP_FILES} Makefile
	rm -f full
	-for i in ${STD_HELP_FILES1} obj.file ${STD_HELP_FILES2} \
	    ${BUILT_HELP_FILES} ${STD_HELP_FILES3}; do \
	    if [ Xintro != X"$$i" ]; then \
		echo ""; \
	    fi; \
	    if [ Xobj.file = X"$$i" ]; then \
		j=obj; \
	    else \
		j=$$i; \
	    fi; \
	    echo "*************"; \
	    echo "* $$j"; \
	    echo "*************"; \
	    echo ""; \
	    cat $$i; \
	done > full

clean:

clobber:
	rm -f ${BUILT_HELP_FILES} full

install: all
	-@if [ ! -d ${TOPDIR} ]; then \
		echo mkdir ${TOPDIR}; \
		mkdir ${TOPDIR}; \
	fi
	-@if [ ! -d ${LIBDIR} ]; then \
		echo mkdir ${LIBDIR}; \
		mkdir ${LIBDIR}; \
	fi
	-@if [ ! -d ${HELPDIR} ]; then \
		echo mkdir ${HELPDIR}; \
		mkdir ${HELPDIR}; \
	fi
	@for i in ${STD_HELP_FILES} ${BUILT_HELP_FILES} full; do \
		echo rm -f ${HELPDIR}/$$i; \
		rm -f ${HELPDIR}/$$i; \
		echo cp $$i ${HELPDIR}; \
		cp $$i ${HELPDIR}; \
		echo chmod 0444 ${HELPDIR}/$$i; \
		chmod 0444 ${HELPDIR}/$$i; \
	done
	rm -f ${HELPDIR}/obj
	cp obj.file ${HELPDIR}/obj
	chmod 0444 ${HELPDIR}/obj
