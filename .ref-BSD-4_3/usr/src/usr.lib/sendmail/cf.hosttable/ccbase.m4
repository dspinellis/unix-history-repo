divert(10)
#
#  Sendmail
#  Copyright (c) 1983  Eric P. Allman
#  Berkeley, California
#
#  Copyright (c) 1983 Regents of the University of California.
#  All rights reserved.  The Berkeley software License Agreement
#  specifies the terms and conditions for redistribution.
#
#	@(#)ccbase.m4	5.5 (Berkeley) 10/2/85
#
divert(0)
#################################################
#
#	General configuration information
#
#################################################

######################
#   General Macros   #
######################

# our arpanet gateway
DABerkeley.EDU
CABerkeley ucbc70 c70 UCB-C70 u UCB UCB-VAX

# local domain names
DDBERKELEY
CDBerkeley

# major relay host
DRLAPIS
CRucbjade JADE H

# my official hostname
Dj$w

# known SMTP hosts in this domain
CSucbjade jade
# CSucbruby ruby

# known top-level domains
CTLOCAL UUCP BITNET ARPA CSNET

include(base.m4)

#######################
#   Rewriting rules   #
#######################

##### special local conversions
S6
R$*<@$*$=D>$*		$1<@$2LOCAL>$4			convert local domain
R$*<@$*$=D.$=A>$*	$1<@$2LOCAL>$5
R$*<@$*$=D.$=A.ARPA>$*	$1<@$2LOCAL>$5
R$+%$*$=D<@$=A>		$1<@$2LOCAL>			hack for % syntax
R$+%$*$=D<@$=A.ARPA>	$1<@$2LOCAL>
R$*<@$+.$=T.$=T>$*	$1<@$2.$3>$5			UUCP/Bitnet top level
