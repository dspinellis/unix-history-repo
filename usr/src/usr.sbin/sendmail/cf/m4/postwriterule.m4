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
#	@(#)postwriterule.m4	1.9 (Berkeley) 2/15/89
#
divert(0)
#################################
#  Final Output Post-rewriting  #
#################################
S4

R@			$@				handle <> error addr

# resolve numeric addresses to name if possible
R$*<@[$+]>$*		$:$1<@$[[$2]$]>$3		lookup numeric internet addr

# externalize local domain info
R$*<$+>$*		$1$2$3				defocus
R@$+:@$+:$+		@$1,@$2:$3			<route-addr> canonical

# UUCP must always be presented in old form
R$+@$-.UUCP		$2!$1				u@h.UUCP => h!u

# delete duplicate local names
R$+%$=w@$=w		$1@$w				u%host@host => u@host
R$+%$=w@$=w.$D		$1@$w				u%host@host => u@host
