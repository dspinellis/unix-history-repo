#
# $Id: Makefile,v 5.2 90/06/23 22:21:02 jsp Rel $
#
# Copyright (c) 1990 Jan-Simon Pendry
# Copyright (c) 1990 Imperial College of Science, Technology & Medicine
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# %sccs.include.redist.sh%
#
#	@(#)Makefile	5.1 (Berkeley) %G%
#

SHELL = /bin/sh
PROGS = amd amq mk-amd-map
MAKE = make
MKARGS = -f Makefile.top

all: FRC
	@for prog in ${PROGS}; do \
		echo Making $$prog; \
		(cd $$prog; ${MAKE} $$prog ) \
	done

${PROGS}: FRC
	@cd $@; ${MAKE} $@

count clean install lint: FRC
	@for prog in ${PROGS}; do \
		echo $@\'ing $$prog; \
		(cd $$prog; ${MAKE} $@) \
	done

pat print tarfile sharfile filelist: FRC
	@$(MAKE) ${MKARGS} ${MFLAGS} $@ OS=undef PROG=doc

FRC:
