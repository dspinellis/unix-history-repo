divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

divert(0)
VERSIONID(`@(#)bestmx_is_local.m4	8.1 (Berkeley) %G%')
divert(-1)

LOCAL_CONFIG
Kbestmx bestmx

LOCAL_NET_CONFIG

# If we are the best MX for a site, then we want to accept
# its mail as local.  We assume we've already weeded out mail to
# UUCP sites which are connected to us, which should also have
# listed us as their best MX.
#
# Warning: this may generate a lot of extra DNS traffic -- a
# lower cost method is to list all the expected best MX hosts
# in $=w.  This should be fine (and easier to administer) for
# low to medium traffic hosts.

R$* < @ $* > $*			$: $1 < @ $2 @@ $(bestmx $2 $) > $3
R$* < @ $* @@ $=w . > $*	$#local $: $1
R$* < @ $* @@ $* > $*		$: $1 < @ $2 > $4
