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
#	@(#)fncpm.m4	5.2 (Berkeley) 8/2/85
#
divert(0)
############################################################
############################################################
#####
#####		(Fake) Arpanet Mailer specification
#####
############################################################
############################################################

#Marpa,	P=/usr/lib/mailers/arpa, F=sDFMu, S=15, R=15, A=sendarpa $f $h $u
Marpa,	P=/usr/net/bin/sendberkmail, F=sDFMu, S=15, R=15,
	A=sendberkmail -m $G -h $c -t $u@$h

S15
R$+			$:$>5$1				convert to old form
R$+<@$+.CC>		$2.$1<@$A>			externalize comp ctr
R$+:$+			$1.$2				convert colon to dot
R$+<@$->		$@$1<@$2>			fine....
R$+			$@$1<@$A>			tack on global info
