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
#	@(#)berkm.m4	1.2 (Berkeley) 3/6/86
#
divert(0)
############################################################
############################################################
#####
#####		Berknet Mailer specification
#####
############################################################
############################################################

Mberk,	P=/usr/net/bin/sendberkmail, F=fsDFMC, S=12, R=22, M=100000,
	A=sendberkmail -m $h -h $c -t $u

S12
R$+			$:$>5$1				convert to old style
R$-:$+			$@$1:$2				old berknet as is
R$+<@$+>		$@$1<@$2>			don't modify arpanet
R$-!$+			$@$1!$2				don't modify uucp
R<@$+>			$@<@$1>				don't modify <routeaddr>
R$+			$@$B:$1				make others relative

S22
R$+			$:$>5$1				convert to old style
R$*%$-			$:$>9$1%$2			convert % to @
