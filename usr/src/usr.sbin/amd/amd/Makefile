#
# $Id: Makefile,v 5.2 90/06/23 22:21:10 jsp Rel $
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

MKARG = -f ../Makefile.top PROG=amd
SHELL = /bin/sh

amd: FRC
	@${MAKE} ${MKARG} $@

install count clean lint: FRC
	@${MAKE} ${MKARG} $@

FRC:
