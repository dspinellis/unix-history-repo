############################################################
############################################################
#####
#####		SENDMAIL CONFIGURATION FILE
#####
#####	This is the master configuration file for the computer
#####	center.  Any other messages get bounced to ucbvax.
#####
#####		@(#)jade.mc	4.1		7/25/83
#####
############################################################
############################################################



############################################################
###	local info
############################################################

# internet hostname
Cwucbjade jade H cfo-h ucbcfo-h LOCAL

# the guy upstairs
DUUCBVAX

# known berknet hosts
CZucbcfo-a cfo-a A
CZucbcfo-b cfo-b B
CZucbcfo-c cfo-c C
CZucbcfo-d cfo-d D
CZucbcfo-e cfo-e E
CZucbcfo-f cfo-f F
CZucbcfo-g cfo-g G
CZucbcfo-h cfo-h H ucbjade jade
CZucbcfo-q cfo-q Q ucbruby ruby
CZucbsrc Src S
CZucbcfo-2 cfo-2 2 ucbtopaz topaz
CZucbcfo-3 cfo-3 3 ucbcoral coral
CZucbcfo-4 cfo-4 4 ucbamber amber
CZucbcfo-5 cfo-5 5 ucbivory ivory
CZucbcfo-6 cfo-6 6 ucblapis lapis

# known computer center hosts
include(cchosts.m4)

include(ccbase.m4)

include(zerobase.m4)

################################################
###  Machine dependent part of ruleset zero  ###
################################################

# resolve names that can go via the ethernet
R$*<@$*$=S>$*		$#ether$@$3$:$1<@$2$3>$4	user@etherhost

#R$*<@$~C.LOCAL>$*	$#error$:Host unknown		user@unknown.CC

# resolve berknet names
R<@$=Z>:$+		$#berk$@$1$:$2			@berkhost: ...
R$+<@$=Z>		$#berk$@$2$:$1			user@berknethost

# resolve bitnet names
R$+<@$+.BITNET>		$#berk$@G$:$2=$1		Bitnet

# other non-local names will be kicked upstairs
R$*<@$+>$*		$#ether$@$U$:$1<@$2>$3		user@host

# remaining names must be local
R$+			$#local$:$1			everything else

############################################################

include(etherm.m4)
include(berkm.m4)
