############################################################
############################################################
#####
#####		UUCP Mailer specification
#####
#####		%W%	%Y%	%G%
#####
############################################################
############################################################

ifdef(`m4COMPAT',, `include(compat.m4)')

Muucp	/usr/bin/uux		sDFhuU	13  23	uux - -r $h!rmail ($u)

S13
R$+			$:$>5$1				convert to old style
R$=U!$+			$2				strip local name
R$+			$:$U!$1				stick on our host name
R$=U!$=R:$+		$:$1!$3				ucbvax!ucbvax:xxx

S23
R$+			$:$>5$1				convert to old style
