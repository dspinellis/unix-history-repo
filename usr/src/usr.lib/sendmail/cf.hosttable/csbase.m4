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
#	@(#)csbase.m4	5.16 (Berkeley) 11/4/85
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
CABerkeley UCB ucbc70 c70 UCB-C70 u UCB-VAX ucbvax

# local domain names
DDBERKELEY.EDU
CDBERKELEY UCB ucbc70 c70 ucb-c70 u UCB-VAX ucbvax

# major relay host
DRUCBVAX
CRucbvax vax k

# my official hostname
Dj$w

# known SMTP/ethernet hosts (this domain only) -- only $R need be complete

CSucbarmstrong armstrong
CSucbarpa arpa
CSucbbach bach ucbstatvax statvax
CSucbbass bass
CSucbbell bell
CSucbbizet bizet
CSucbbob bob
CSucbbrahms brahms mathstat
CSucbbud bud
CSucbbuddy buddy
CSucbcad cad cad-a
CSucbcalder calder
CSucbcartan cartan
CSucbchip chip
CSucbcogsci cogsci
CSucbcorona corona
CSucbcory cory
CSucbdab dab
CSucbdali dali
CSucbdavinci davinci
CSucbdavis davis
CSucbdean dean
CSucbdeforest deforest
CSucbdegas degas
CSucbdim dim
CSucbdorothy dorothy
CSucbear ear
CSucbeast east beast
CSucbedison edison
CSucbernie ernie
CSucbesvax esvax
CSucbeuler euler
CSucbfranklin franklin
CSucbfranny franny
CSucbheine heine
CSucbholden holden
CSucbhuffman huffman
CSucbic ic ucbic-ec ic-ec cad-b cad-ic
CSucbicw icw ucbicw-ec icw-ec cad-c cad-icw
CSucbimage 
CSucbingres ingres
CSucbjan jan
CSucbjason jason
CSucbji ji
CSucbjoule joule
CSucbkepler kepler
CSucbkim kim
CSucbmarconi marconi
CSucbmatisse matisse
CSucbmaxwell maxwell
CSucbmedea medea
CSucbmerlin merlin
CSucbmike mike
CSucbmiro miro
CSucbmolson molson
CSucbmonet monet
CSucbmote mote
CSucbnewton newton
CSucbnyquist nyquist
CSucbokeeffe okeeffe
CSucboz oz
CSucbrenoir renoir
CSucbrob rob
CSucbrobert robert
CSucbseymour seymour
CSucbshadow shadow
CSucbshannon shannon
CSucbsilvia silvia
CSucbtuborg tuborg
CSucbugs bugs ucbbugs ugs
CSucbvangogh vangogh
CSucbvax
CSucbweyl weyl
CSucbzeus zeus
CSucbzooey zooey
CSucbzworykin zworykin

# known top-level domains
CTLOCAL ARPA EDU GOV COM MIL
CTUUCP BITNET CSNET DEC

include(base.m4)

#######################
#   Rewriting rules   #
#######################

##### special local conversions
S6
R$*<@$*$=D>$*		$1<@$2LOCAL>$4			convert local domain
R$*<@$*$=D.ARPA>$*	$1<@$2LOCAL>$4
R$*<@$*$=D.EDU>$*	$1<@$2LOCAL>$4
R$+%$+<@LOCAL>		$>9$1%$2			Hack for % syntax.
R$*<@$+.$=T.$=T>$*	$1<@$2.$3>$5			make UUCP top level
R$*<@$*$=C.$=T>$*	$1<@$2$3>$5			adjust CC hosts (ucbvax)
R$*<@$*$=C>$*		$1<@$2$3.CC>$4			tack on .CC temporarily
R$*<@$*$=S.UUCP>$*	$1<@$2$3.LOCAL>$4		trap u@etherhost.UUCP
R$*<@ucsfcgl.UUCP>$*	$1<@ucsf-cgl.ARPA>$2		ucsfcgl now on Internet

################################
#   Change rightmost % to @.   
S9
R$*%$*			$1@$2				First make them all @'s
R$*@$*@$*		$1%$2@$3			Undo all but the last.
R$*@$*			$@$1<@$2>			Put back the brackets.
#
###############################

include(etherm.m4)
