############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####		@(#)cad.mc	3.10		2/24/83
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
CWvodervax
CWlbl-`unix'
CWlbl-h

include(csbase.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names that we can handle locally
R<@$=W.UUCP>$+		$#uucp$@$1$:$2			@host.UUCP: ...
R$+<@$=W.UUCP>		$#uucp$@$2$:$1			user@host.UUCP

# resolve names that can go via the ethernet
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$R$:$1<@$2>$3		user@some.where

# remaining names must be local
R$+			$#local$:$1			everything else

include(uucpm.m4)
