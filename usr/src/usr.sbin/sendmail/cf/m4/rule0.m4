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
#	@(#)rule0.m4	1.10 (Berkeley) 1/3/89
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
