divert(-1)
#
# Copyright (c) 1983, 1995 Eric P. Allman
# Copyright (c) 1988, 1993
#	The Regents of the University of California.  All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)

VERSIONID(`@(#)nullrelay.m4	8.8 (Berkeley) %G%')

#
#  This configuration applies only to relay-only hosts.  They send
#  all mail to a hub without consideration of the address syntax
#  or semantics, except for adding the hub qualification to the
#  addresses.
#
#	This is based on a prototype done by Bryan Costales of ICSI.
#

######################################################################
######################################################################
#####
#####			REWRITING RULES
#####
######################################################################
######################################################################

###########################################
###  Rulset 3 -- Name Canonicalization  ###
###########################################
S3

# handle null input and list syntax (translate to <@> special case)
R$@			$@ <@>
R$*:;$*			$@ $1 :; <@>

# basic textual canonicalization -- note RFC733 heuristic here
R$*<$*>$*<$*>$*		$2$3<$4>$5			strip multiple <> <>
R$*<$*<$+>$*>$*		<$3>$5				2-level <> nesting
R$*<>$*			$@ <@>				MAIL FROM:<> case
R$*<$+>$*		$2				basic RFC821/822 parsing

ifdef(`_NO_CANONIFY_', `dnl',
`# eliminate local host if present
R@ $=w $=: $+		$@ @ $M $2 $3			@thishost ...
R@ $+			$@ @ $1				@somewhere ...

R$+ @ $=w		$@ $1 @ $M			...@thishost
R$+ @ $+		$@ $1 @ $2			...@somewhere

R$=w ! $+		$@ $2 @ $M			thishost!...
R$+ ! $+		$@ $1 ! $2 @ $M			somewhere ! ...

R$+ % $=w		$@ $1 @ $M			...%thishost
R$+ % $+		$@ $1 @ $2			...%somewhere

R$+			$@ $1 @ $M			unadorned user')


######################################
###   Ruleset 0 -- Parse Address   ###
######################################

S0

R$*:;<@>		$#error $@ USAGE $: "list:; syntax illegal for recipient addresses"

# pass everything else to a relay host
R$*			$#_RELAY_ $@ $H $: $1

#
######################################################################
######################################################################
#####
`#####			MAILER DEFINITIONS'
#####
######################################################################
######################################################################
undivert(7)dnl
