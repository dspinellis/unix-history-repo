divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  The following is a generic domain file.  You should be able to
#  use it anywhere.  If you want to customize it, copy it to a file
#  named with your domain and make the edits; then, copy the appropriate
#  .mc files and change DOMAIN(generic) to reference your updated domain
#  files.
#
divert(0)
VERSIONID(`@(#)generic.m4	8.1 (Berkeley) %G%')
define(`confFORWARD_PATH', `$z/.forward.$w:$z/.forward')dnl
define(`confCW_FILE', `-o /etc/sendmail.cw')dnl
FEATURE(redirect)dnl
FEATURE(use_cw_file)dnl
