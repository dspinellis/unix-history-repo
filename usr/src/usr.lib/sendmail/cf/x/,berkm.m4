############################################################
############################################################
#####
#####		Berknet Mailer specification
#####
#####		%W%	%Y%	%G%
#####
############################################################
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

Mberk	/usr/net/bin/sendberkmail fsAC	12  22	sendberkmail -m $h -h $c -t $u

S12
R$+			$:$>5$1				convert to old style
R$-:$+			$@$1:$2				old berknet as is
R$+@$+			$@$1@$2				don't modify arpanet
R$-!$+			$@$1!$2				don't modify uucp
R$+			$@$B:$1				make others relative

S22
R$+			$:$>5$1				convert to old style
