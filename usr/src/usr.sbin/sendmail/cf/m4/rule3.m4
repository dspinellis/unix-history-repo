divert(10)
#
# Copyright (c) 1983 Eric P. Allman
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)rule3.m4	1.18 (Berkeley) 2/15/89
#
divert(0)
###########################
#  Name Canonicalization  #
###########################
S3

# handle "from:<>" special case
R$*<>$*			$@@				turn into magic token

# basic textual canonicalization -- note RFC733 heuristic here
R$*<$*<$*<$+>$*>$*>$*	$4				3-level <> nesting
R$*<$*<$+>$*>$*		$3				2-level <> nesting
R$*<$+>$*		$2				basic RFC821/822 parsing

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
R$+%$+			$:$>9$1%$2			user%host
R$+<@$+>		$@$>6$1<@$2>			already canonical
R$-.$+			$@$>6$2<@$1>			host.user


#################################
#   special local conversions   #
#################################

S6
R$*<@$=w>$*		$:$1<@$w>$3			get into u@$w form
R$*<@$=w.$D>$*		$:$1<@$w>$3
R$*<@$=U.UUCP>$*	$:$1<@$w>$3

################################
#   Change rightmost % to @.   #
################################

S9
R$*%$*			$1@$2				First make them all @'s.
R$*@$*@$*		$1%$2@$3			Undo all but the last.
R$*@$*			$@$1<@$2>			Put back the brackets.

