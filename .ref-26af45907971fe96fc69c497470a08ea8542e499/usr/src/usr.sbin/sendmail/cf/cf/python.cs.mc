divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#

include(`../m4/cf.m4')
VERSIONID(`@(#)python.cs.mc	6.1 (Berkeley) %G%')
OSTYPE(bsd4.4)dnl
DOMAIN(cs.exposed)dnl
define(`LOCAL_RELAY', vangogh.CS.Berkeley.EDU)dnl
define(`MASQUERADE_NAME', vangogh.CS.Berkeley.EDU)dnl
MAILER(local)dnl
MAILER(smtp)dnl

# accept mail sent to the domain head
DDBostic.COM

LOCAL_RULE_0
# accept mail sent to the domain head
R< @ $D . > : $*		$@ $>7 $1		@here:... -> ...
R$* $=O $* < @ $D . >		$@ $>7 $1 $2 $3		...@here -> ...
R$* < @ $D . >			$#local $: $1		user@here -> user
