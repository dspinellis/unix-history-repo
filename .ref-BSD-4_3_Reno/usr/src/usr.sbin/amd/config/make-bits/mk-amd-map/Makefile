#
# $Id: Makefile,v 5.2 90/06/23 22:21:27 jsp Rel $
#
# Copyright (c) 1990 Jan-Simon Pendry
# Copyright (c) 1990 Imperial College of Science, Technology & Medicine
# Copyright (c) 1990 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# Redistribution and use in source and binary forms are permitted provided
# that: (1) source distributions retain this entire copyright notice and
# comment, and (2) distributions including binaries display the following
# acknowledgement:  ``This product includes software developed by the
# University of California, Berkeley and its contributors'' in the
# documentation or other materials provided with the distribution and in
# all advertising materials mentioning features or use of this software.
# Neither the name of the University nor the names of its contributors may
# be used to endorse or promote products derived from this software without
# specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.1 (Berkeley) 7/19/90
#

MKARG = -f ../Makefile.top PROG=mk-amd-map
SHELL = /bin/sh

mk-amd-map: FRC
	@${MAKE} ${MKARG} $@

install clean count lint: FRC
	@${MAKE} ${MKARG} $@

FRC:
