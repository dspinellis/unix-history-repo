############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####		@(#)cad.mc	4.2		8/21/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
Cwucbcad cad cad-a

# UUCP name
DUucbcad
CUucbcad

# UUCP hosts that we talk to
CWteklabs
CWtekcad
CWtekcrd
CWtektronix tektroni tektron
CWboulder
CWhpda
CWwheps
CWharpo
CWchico
CWmi-cec
CWfortune
CWvodervax voderva
CWcaddyvax caddyva
CWlbl-csam lbl-`unix'
CWlblh
CWmasscomp masscom
CWpenelope penelop
CWulysses
CWjupiter

include(csbase.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names that we can handle locally
R<@$=W.UUCP>$+		$#uucp$@$1$:$2			@host.UUCP: ...
R$+<@$=W.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve names that can go via the ethernet
R$*<@$*$=S.LOCAL>$*	$#ether$@$3$:$1<@$2$3.$D>$4	user@etherhost

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$R$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

include(uucpm.m4)
