divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)uucpdomain.m4	8.5 (Berkeley) %G%')
divert(-1)


PUSHDIVERT(6)
Kuudomain ifelse(_ARG_, `', `hash -o /etc/uudomain', `_ARG_')
POPDIVERT


PUSHDIVERT(2)
# handle UUCP mapping
R$* < @ $+ .UUCP. > $*		$: $1 < @ $(uudomain $2 $: $2.UUCP. $) > $3
POPDIVERT
