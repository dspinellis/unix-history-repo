divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

include(`../m4/cf.m4')
VERSIONID(`@(#)huginn.cs.mc	8.1 (Berkeley) %G%')
OSTYPE(hpux)dnl
DOMAIN(Berkeley)dnl
MASQUERADE_AS(CS.Berkeley.EDU)dnl
FEATURE(notsticky)dnl
MAILER(local)dnl
MAILER(smtp)dnl
DDBerkeley.EDU

# hosts for which we accept and forward mail (must be in .Berkeley.EDU)
CF CS
FF/etc/sendmail.cw

LOCAL_RULE_0
R< @ $=F . $D . > : $*		$@ $>7 $2		@here:... -> ...
R$* $=O $* < @ $=F . $D . >	$@ $>7 $1 $2 $3		...@here -> ...

R$* < @ $=F . $D . >		$#local $: $1		use UDB
