#
# help - makefile for calc help files
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
HELPDIR= /usr/local/lib/calc/help
# how to build a directory
MKDIR=mkdir -p
#MKDIR=mkdir

# The header file, along with these files form the full help file.
#
STD_HELP_FILES=intro command expression define variable statement \
	operator types mat list file builtin config interrupt \
	history usage credit bindings
FULL_HELP_FILES=header overview stdlib environment todo credit help

SHELL= /bin/sh

all: ${STD_HELP_FILES} ${FULL_HELP_FILES} full obj.file

full: ${FULL_HELP_FILES}
	rm -f full
	touch full
	chmod +w full
	@for i in ${FULL_HELP_FILES}; do \
		echo '' >> full; \
		echo '' >> full; \
		echo "cat $$i >> full"; \
		cat $$i >> full; \
	done
	chmod 0444 full

bindings: ../lib/bindings
	rm -f bindings
	cp ../lib/bindings bindings
	chmod 0444 bindings

stdlib: ../lib/README
	rm -f stdlib
	cp ../lib/README stdlib
	chmod 0444 stdlib

clean:

clobber:
	rm -f full bindings stdlib

install: all
	-@if [ ! -d ${HELPDIR} ]; then \
		echo ${MKDIR} ${HELPDIR}; \
		${MKDIR} ${HELPDIR}; \
	fi
	@for i in ${STD_HELP_FILES} ${FULL_HELP_FILES}; do \
		echo rm -f ${HELPDIR}/$$i; \
		rm -f ${HELPDIR}/$$i; \
		echo chmod 0444 $$i; \
		chmod 0444 $$i; \
		echo cp $$i ${HELPDIR}; \
		cp $$i ${HELPDIR}; \
	done
	rm -f ${HELPDIR}/obj
	chmod 0444 obj.file
	cp obj.file ${HELPDIR}/obj
