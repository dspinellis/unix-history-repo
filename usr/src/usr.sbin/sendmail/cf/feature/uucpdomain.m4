divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)uucpdomain.m4	6.2 (Berkeley) %G%')
divert(-1)


PUSHDIVERT(6)
Kuudomain ifelse(_ARG_, `', `hash /etc/uudomain -o', `_ARG_')
POPDIVERT


PUSHDIVERT(8)
# handle UUCP mapping
R$* < @ $+ .UUCP > $*		$: $1 < @ $(uudomain $2 $: $2.UUCP $) > $3
POPDIVERT
