divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)
VERSIONID(`@(#)Berkeley.EDU.m4	8.6 (Berkeley) %G%')
DOMAIN(berkeley-only)dnl
define(`BITNET_RELAY', `CMSA.Berkeley.EDU')dnl
define(`confFORWARD_PATH', `$z/.forward.$w:$z/.forward')dnl
define(`confCW_FILE', `-o /etc/sendmail.cw')dnl
FEATURE(redirect)dnl
FEATURE(use_cw_file)dnl
