divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)bitdomain.m4	6.3 (Berkeley) %G%')
divert(-1)


PUSHDIVERT(6)
Kbitdomain ifelse(_ARG_, `', `hash /etc/bitdomain -o', `_ARG_')
POPDIVERT


PUSHDIVERT(8)
# handle BITNET mapping
R$* < @ $+ .BITNET > $*		$: $1 < @ $(bitdomain $2 $: $2.BITNET $) > $3
POPDIVERT
