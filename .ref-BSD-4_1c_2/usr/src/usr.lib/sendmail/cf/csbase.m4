#################################################
#
#	General configuration information
#
#	@(#)csbase.m4	3.13		3/5/83
#
#################################################

######################
#   General Macros   #
######################

# our arpanet gateway
DABerkeley
CABerkeley UCB ucbc70 c70 UCB-C70 u UCB-VAX ucbvax

# local domain names
DDBERKELEY
CDBERKELEY UCB ucbc70 c70 ucb-c70 u UCB-VAX ucbvax

# major relay host
DRUCBVAX
CRucbvax vax k

# known SMTP/ethernet hosts (this domain only) -- only $R need be complete
CSucbarpa arpa
CSucbcad cad cad-a
CSucbcalder calder
CSucbdali dali
CSucbdegas degas
# CSucbear ear
CSucbernie ernie
# CSucbesvax esvax
# CSucbingres ingres
CSucbkim kim
CSucbmatisse matisse
CSucbmedea medea
CSucbmiro miro
CSucbmonet monet
CSucboz oz
CSucbic ic ucbic-ec ic-ec cad-b cad-ic
CSucbicw icw ucbicw-ec icw-ec cad-c cad-icw
CSucbvax

# known top-level domains
CTLOCAL ARPA UUCP BITNET CC

include(base.m4)

#######################
#   Rewriting rules   #
#######################

##### special local conversions
S6
R$*<@$*$=D>$*		$1<@$2LOCAL>$4			convert local domain
R$*<@$*$=D.ARPA>$*	$1<@$2LOCAL>$4
R$*<@$+.$=T.$=T>$*	$1<@$2.$3>$5			make UUCP top level
R$*<@$+.$=T.$=T>$*	$1<@$2.$3>$5			make UUCP top level
R$*<@$*$=C.$=T>$*	$1<@$2$3>$5			adjust CC hosts (ucbvax)
R$*<@$*$=C>$*		$1<@$2$3.CC>$4			adjust CC hosts (ucbvax)

include(localm.m4)
include(etherm.m4)
