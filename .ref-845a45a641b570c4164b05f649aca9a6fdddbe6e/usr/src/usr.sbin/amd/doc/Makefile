#
# $Id: Makefile,v 5.2 90/06/23 22:21:25 jsp Rel $
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
#	@(#)Makefile	5.4 (Berkeley) %G%
#

PROG = amdref
SRCS = amdref.texinfo

${PROG}: amdref.dvi

amdref.dvi: ${SRCS}
	-tex amdref; tex amdref

clean:
	-rm -f amdref.aux amdref.cp amdref.dvi amdref.fn amdref.ky \
		amdref.log amdref.pg amdref.toc amdref.tp amdref.vr
