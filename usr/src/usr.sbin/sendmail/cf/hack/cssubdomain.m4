divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)
VERSIONID(`@(#)cssubdomain.m4	6.1 (Berkeley) %G%')

divert(2)
# find possible (old & new) versions of our name via short circuit hack
# (this code should exist ONLY during the transition from .Berkeley.EDU
#  names to .CS.Berkeley.EDU names -- probably not more than a few months)
R$* < @ $=w .CS.Berkeley.EDU > $*	$: $1 < @ $j > $3
R$* < @ $=w .Berkeley.EDU> $*		$: $1 < @ $j > $3
divert(0)
