divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

VERSIONID(`@(#)use_cw_file.m4	2.4 (Berkeley) %G%')

# if defined, the sendmail.cf will read the /etc/sendmail.cw file
# to find alternate names for this host.  Typically only used when
# several hosts have been squashed into one another at high speed.

define(`USE_CW_FILE', `')

divert(0)
