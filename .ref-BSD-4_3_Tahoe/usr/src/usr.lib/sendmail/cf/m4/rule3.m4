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
#	@(#)rule3.m4	1.16 (Berkeley) 4/8/88
#
divert(0)
###########################
#  Name Canonicalization  #
###########################
S3

# handle "from:<>" special case
R<>			$@@				turn into magic token

# basic textual canonicalization -- note RFC733 heuristic here
R$*<$*<$*<$+>$*>$*>$*	$4				3-level <> nesting
R$*<$*<$+>$*>$*		$3				2-level <> nesting
R$*<$+>$*		$2				basic RFC821/822 parsing
R$+ at $+		$1@$2				"at" -> "@" for RFC 822

# make sure <@a,@b,@c:user@d> syntax is easy to parse -- undone later
R@$+,$+			@$1:$2				change all "," to ":"

# localize and dispose of route-based addresses
R@$+:$+			$@$>6<@$1>:$2			handle <route-addr>

# more miscellaneous cleanup
R$+			$:$>8$1				host dependent cleanup
R$+:$*;@$+		$@$1:$2;@$3			list syntax
R$+:$*;			$@$1:$2;			list syntax
R$+@$+			$:$1<@$2>			focus on domain
R$+<$+@$+>		$1$2<@$3>			move gaze right
R$+<@$+>		$@$>6$1<@$2>			already canonical

# convert old-style addresses to a domain-based address
R$+^$+			$1!$2				convert ^ to !
R$-!$+			$@$>6$2<@$1.UUCP>		resolve uucp names
R$+.$-!$+		$@$>6$3<@$1.$2>			domain uucps
R$+!$+			$@$>6$2<@$1.UUCP>		uucp subdomains
R$-:$+			$@$>6$2<@$1>			host:user
R$-=$+			$@$>6$2<@$1.BITNET>		resolve bitnet names
R$+%$+			$:$>9$1%$2			user%host
R$+<@$+>		$@$>6$1<@$2>			already canonical
R$-.$+			$@$>6$2<@$1>			host.user


#################################
#   special local conversions   #
#################################

S6
R$*<@$=w>$*		$:$1<@$w>$3			get into u@$w form
R$*<@$=w.$D>$*		$:$1<@$w>$3
R$*<@$=w.EDU>$*		$:$1<@$w>$3
R$*<@$=U.UUCP>$*	$:$1<@$w>$3

################################
#   Change rightmost % to @.   #
################################

S9
R$*%$*			$1@$2				First make them all @'s.
R$*@$*@$*		$1%$2@$3			Undo all but the last.
R$*@$*			$@$1<@$2>			Put back the brackets.

