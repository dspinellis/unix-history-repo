divert(-1)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
divert(0)

VERSIONID(@(#)proto.m4	2.2 (Berkeley) %G%)


##################
#   local info   #
##################

ifdef(`_USE_CW_FILE_',
`# file containing internet aliases in our primary domain
Fw/etc/sendmail.cw', `dnl')

ifdef(`UUCP_RELAY',
`# UUCP relay host
CONCAT(DU, UUCP_RELAY)
')dnl
ifdef(`BITNET_RELAY',
`#  BITNET relay host
CONCAT(DB, BITNET_RELAY)
')dnl
ifdef(`CSNET_RELAY',
`# CSNET relay host
CONCAT(DC, CSNET_RELAY)
')dnl
# my official hostname ($w or $w.$D)
Dj$w

# who I masquerade as (can be $j)
CONCAT(DM, ifdef(`MASQUERADE_NAME', MASQUERADE_NAME, $j))

# who I send unqualified names to (null means deliver locally)
CONCAT(DR, ifdef(`LOCAL_RELAY', LOCAL_RELAY))


ifdef(`UUCP_NAME',
# uucp hostnames
UUCP_NAME
UUCP_ALIASES

# local UUCP connections
`include(UUCP_HOSTS_FILE)')

# operators that cannot be in local usernames (i.e., network indicators)
CO @ % !

# location of user database file (null means no lookup)
ifdef(`USERDB_FILE', `CONCAT(`OU', USERDB_FILE)', `dnl')

ifdef(`_NO_WILDCARD_MX_',
`# we can guarantee no wildcard MX records matching our domain
Ow', `dnl')dnl

include(`../m4/version.m4')

#ifdef(`INTERNET_RELAY',
#`include(../sitedep/nicregistered.m4)')
include(`../m4/boilerplate.m4')

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
`S3

# handle "from:<>" special case
R$*<>$*			$@@				turn into magic token

# basic textual canonicalization -- note RFC733 heuristic here
R$*<$*<$*<$+>$*>$*>$*	$4				3-level <> nesting
R$*<$*<$+>$*>$*		$3				2-level <> nesting
R$*<$+>$*		$2				basic RFC821/822 parsing

# make sure <@a,@b,@c:user@d> syntax is easy to parse -- undone later
R@$+,$+			@$1:$2				change all "," to ":"

# localize and dispose of route-based addresses
R@$+:$+			$@$>6<@$1>:$2			handle <route-addr>

# find focus for list syntax
R$+:$*;@$+		$@$>6$1:$2;<@$3>		list syntax
R$+:$*;			$@$1:$2;			list syntax

# find focus for @ syntax addresses
R$+@$+			$:$1<@$2>			focus on domain
R$+<$+@$+>		$1$2<@$3>			move gaze right
R$+<@$+>		$@$>6$1<@$2>			already canonical

# convert old-style addresses to a domain-based address
R$-!$+			$@$>6$2<@$1.UUCP>		resolve uucp names
R$+.$-!$+		$@$>6$3<@$1.$2>			domain uucps
R$+!$+			$@$>6$2<@$1.UUCP>		uucp subdomains

# if we have % signs, take the rightmost one
R$*%$*			$1@$2				First make them all @s.
R$*@$*@$*		$1%$2@$3			Undo all but the last.
R$*@$*			$@$>6$1<@$2>			Insert < > and finish

# else we must be a local name


###############################################
###  Ruleset 6 -- bottom half of ruleset 3  ###
###############################################

#
#  At this point, everything should be in a local_part@domain format.
#  This ruleset is primarily responsible for 

S6
'
undivert(2)dnl

# handle special cases for local names
R$*<@$=w>$*		$:$1<@$w>$3			no domain at all
R$*<@$=w.UUCP>$*	$:$1<@$w>$3			.UUCP domain
R$*<@$=U.UUCP>$*	$:$1<@$w>$3			UUCP-specific names

# pass to name server to make hostname canonical
R$* < @ $+ > $*		$: $1 < @ $[ $2 $] > $3		then do anything

# hack to add local domain if nothing sofar has done so
R$* < @ $- > $*		$: $1 < @ $2 @ $w > $3		throw on local host
R$* < @ $- @ $- $* > $*	$: $1 < @ $2 $4 > $5		strip host part only


##################################################
###  Ruleset 4 -- Final Output Post-rewriting  ###
##################################################
S4

R@			$@				handle <> error addr

# resolve numeric addresses to name if possible
R$*<@[$+]>$*		$:$1<@$[[$2]$]>$3		lookup numeric internet addr

# externalize local domain info
R$*<$+>$*		$1$2$3				defocus
R@$+:@$+:$+		@$1,@$2:$3			<route-addr> canonical

# UUCP must always be presented in old form
R$+@$-.UUCP		$2!$1				u@h.UUCP => h!u

# delete duplicate local names
R$+%$=w@$=w		$1@$w				u%host@host => u@host
#R$+%$=w@$=w.$D		$1@$w				u%host@host => u@host



#############################################################
###   Ruleset 7 -- recanonicalize and call ruleset zero   ###
###		   (used for recursive calls)		  ###
#############################################################

S7
R$*			$: $>3 $1
R$*			$@ $>0 $1


######################################
###   Ruleset 0 -- Parse Address   ###
######################################

S0

# handle numeric address spec
R$*<@[$+]>$*		$:$1<@$[[$2]$]>$3		numeric internet addr
R$*<@[$+]>$*		$#smtp$@[$2]$:$1@[$2]$3		numeric internet spec

#R@			$#error$:Invalid address	handle <> form

# now delete the local info -- note $=O to find characters that cause forwarding
R<@$w>:$*		$@$>7$1				@here:... -> ...
R$*$=O$*<@$w>		$@$>7$1$2$3			...@here -> ...

# short circuit local delivery so forwarded email works
R$+<@$w>		$#local$:$1			local address

undivert(3)dnl

# resolve fake top level domains by forwarding to other hosts
ifdef(`BITNET_RELAY',
`R$*<@$+.BITNET>$*	$#smtp$@$B$:$1<@$2.BITNET>$3		user@host.BITNET',
	`dnl')
ifdef(`CSNET_RELAY',
`R$*<@$+.CSNET>$*	$#smtp$@$C$:$1<@$2.CSNET>$3		user@host.CSNET',
	`dnl')

# forward non-local UUCP traffic to our UUCP relay
R$*<@$*.UUCP>$*		$#smtp$@$U$:$1<@$2.UUCP>		uucp mail

# hide behind our internet relay when talking to people in the .ARPA domain
#R$*<@$*.ARPA>$*	$#smtp-nic$@$2.ARPA$:$1<@$2.ARPA>$3	user@host.ARPA

# but speak domains to them if they speak domains too
R$* < @ $* > $*		$# smtp $@ $2 $: $1 < @ $2 > $3		user@host.domain

# see if we forward local names
R$=L			$# local $: $1			special local names
R$+			$: $1 @ $R
R$+ @			$: $1				no, we don't
R$+ @ $+		$# smtp $@ $2 $: $1		yes, we do

# remaining names must be local
R$+			$#local$:$1			everything else

#
######################################################################
######################################################################
#####
#####			MAILER DEFINITIONS
#####
######################################################################
######################################################################

undivert(7)dnl
