############################################################
############################################################
#####
#####		Berknet Mailer specification
#####
#####		@(#)berkm.m4	3.5		2/24/83
#####
############################################################
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

Mberk,	P=/usr/net/bin/sendberkmail, F=fsDFMC, S=12, R=22, M=100000,
	A=sendberkmail -m $h -h $c -t $u

S12
R$+			$:$>5$1				convert to old style
R$-:$+			$@$1:$2				old berknet as is
R$+<@$+>		$@$1<@$2>			don't modify arpanet
R$-!$+			$@$1!$2				don't modify uucp
R$+			$@$B:$1				make others relative

S22
R$+			$:$>5$1				convert to old style
