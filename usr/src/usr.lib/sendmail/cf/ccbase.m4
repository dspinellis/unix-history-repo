#################################################
#
#	General configuration information
#
#	@(#)ccbase.m4	4.1		7/25/83
#
#################################################

######################
#   General Macros   #
######################

# our arpanet gateway
DABerkeley
CABerkeley ucbc70 c70 UCB-C70 u UCB UCB-VAX

# local domain names
DDCC.BERKELEY
CDCC

# major relay host
DRJADE
CRucbjade JADE H

# my official hostname
Dj$w.CC.Berkeley.ARPA

# known SMTP hosts in this domain
CSucbjade jade
# CSucbruby ruby

# known top-level domains
CTLOCAL UUCP BITNET ARPA

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
R$*<@$+.$=T.$=T>$*	$1<@$2.$3>$5			UUCP/Bitnet top level
R$*<@$=C>$*		$1<@$2.CC>$3			adjust CC hosts

include(localm.m4)
