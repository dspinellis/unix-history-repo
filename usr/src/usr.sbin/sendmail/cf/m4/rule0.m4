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
#	@(#)rule0.m4	1.11 (Berkeley) 2/15/89
#
divert(0)
############################################################
############################################################
#####
#####		RULESET ZERO PREAMBLE
#####
#####	The beginning of ruleset zero is constant through all
#####	configurations.
#####
############################################################
############################################################

S0

# first make canonical
R$*<$*>$*		$1$2$3				defocus
R$+			$:$>3$1				make canonical

# handle special cases
R$*<@[$+]>$*		$:$1<@$[[$2]$]>$3		numeric internet addr
R$*<@[$+]>$*		$#tcp$@[$2]$:$1@[$2]$3		numeric internet spec
R$+			$:$>6$1
R$-<@$w>		$#local$:$1
R@			$#error$:Invalid address	handle <> form

# canonicalize using the nameserver if not internal domain
R$*<@$*.$~I>$*		$:$1<@$[$2.$3$]>$4
R$*<@$->$*		$:$1<@$[$2$]>$3
R$*<@$->$*		$:$1<@$2.$D>$3			if nameserver fails

# now delete the local info
R<@$w>:$*		$@$>0$1				@here:... -> ...
R$*<@$w>		$@$>0$1				...@here -> ...

##################################
#  End of ruleset zero preamble  #
##################################
