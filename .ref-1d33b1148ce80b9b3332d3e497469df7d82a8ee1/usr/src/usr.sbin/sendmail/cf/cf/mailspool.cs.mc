divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#

#
#  This is a Berkeley-specific configuration file for a specific
#  machine in the Computer Science Division at Berkeley, and should
#  not be used elsewhere.   It is provided on the sendmail distribution
#  as a sample only.
#
#  This file is for our mail spool machine.  For a while we were using
#  "root.machinename" instead of "root+machinename", so this is included
#  for back compatibility.
#

include(`../m4/cf.m4')
VERSIONID(`@(#)mailspool.cs.mc	8.2 (Berkeley) %G%')
OSTYPE(sunos4.1)dnl
DOMAIN(CS.Berkeley.EDU)dnl
MAILER(local)dnl
MAILER(smtp)dnl

LOCAL_CONFIG
CDroot sys-custodian

LOCAL_RULE_3
R$=D . $+		$1 + $2
