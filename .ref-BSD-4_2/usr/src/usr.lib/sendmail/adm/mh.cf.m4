##########################################
# sendmail configuration file
#	@(#)cf.m4	3.42		1/1/82
##########################################

include(macros.m4) dnl
include(whoami.m4) dnl

### local hosts on various nets
# arpanet
ifdef(`ANHOST', concat(DA, ANHOST))
# berknet
ifdef(`BNHOST', concat(DB, BNHOST))
# ethernet
ifdef(`ENHOST', concat(DE, ENHOST))
# uucpnet
ifdef(`UNHOST', concat(DU, UNHOST))
# arpanet gateway
DGucbvax
# uucp gateway
DHcbosgd
# IBM gateway
DIg
# Berknet gateway
DJucbvax

### special macros
# my name
DnMAILER-DAEMON
# my official SMTP hostname
ifdef(`UNHOST', concat(Di, UNHOST))
# UNIX header format
DlFrom $g  $d
# delimiter (operator) characters
Do.:%@!^=/

### other special macros are set internally:
# f -- from person
# g -- from person translated by mailer
# u -- the user being sent to
# h -- the host being sent to
# c -- the hop count
# p -- the process id -- for unique names
# t -- the current time (as a base 10 number)
# d -- the sending date (as a ctime string)
# a -- the sending date (in arpanet format)
# b -- the current date (in arpanet format)
# x -- ``signature'' (full name)
# y -- the tty id
# z -- home directory of recipient

### format of headers:
HMail-From:
HDate: $a
HFrom: $g$?x ($x)$.
HFull-Name: $x
HSubject:
# HFrom: $?x$x $.<$g>
# HPosted-Date: $a
# HReceived-Date: $b
# HMessage-Id: <$t.$p.$B@$A>
HVia: $U.uucp (V$v); $b

### name classifications
# arpanet hostnames
CAUCB Berkeley UCB-C70
# list of local host names
CBCSVAX v VAX ernie ucbvax
# berknet hosts on the arpanet
CCu c70
# uucp hostnames
CUcbosgd
# known domains
CDarpa berk uucp ibm
# known SMTP hosts
CSmonet ucb-comet oscar bert ucb-arpa
# known berknet hosts
CHA B C D E F G Q S Src
CHI Ing70 Ingres J IngVAX
CHV CSVAX R ARPAVAX U C70 Y Cory X Onyx
CHO ESVAX M Image Z EECS40
CHN Kim T MathStat P UCBCAD
CHW StatVAX L VLSI K Virus

###  mailers
# local mail -- must be zero
Mlocal	/bin/mail		rlsAmn	$f	...LocalMail -d $u
# program mail -- must be one
Mprog	/bin/csh		lA	$f	...ProgMail -fc $u
# berkeley net mail
Mberk	/usr/bin/uux		fsA	$U!$f	...BerkMail - $J!rmail $h:$u
# arpanet mail
Marpa	/usr/bin/uux		fsuA	$U!$f@$A ...ArpaMail $G!rmail $u@$h
# uucp mail
Muucp	/usr/bin/uux		rsDxmhu	$U!$f	...UucpMail - $h!rmail ($u)
# IBM RJE mail
Mibm	/usr/bin/uux		fxsuA	$U!$f	...IbmMail $H!rmail $h=$u
# SMTP mail over TCP (this appears to be kludged in and need work)
Msmtp	/usr/bin/uux		sAu	$U!$f	...EtherMail ucbvax!rmail ($h:$u)

### rewriting rules
R$+ at $+	$1@$2			change "at" to "@"
R$+.$-@$=D	$#$3$@$2$:$1		resolve user.host@domain
R$=C:$+@$-	$2@$3			delete gateway: on arpanet addresses
R$+@$=A		$G:$1			delete local arpa hosts
R$+@$-		$#arpa$@$2$:$1		resolve arpa mail
R$-=$+		$#ibm$@$1$:$2		resolve IBM mail
R$=S:$+		$#smtp$@$1$:$2		resolve SMTP mail
R$=H.$+		$1:$2			change "." to ":"
R$+^$+		$1!$2			change "^" to "!"
R$-!$=U!$+	$3			delete uucp loops through here
R$-!$+		$#uucp$@$1$:$2		resolve uucp mail
R$-:$-:$+	$2:$3			delete multiple berk hosts
R$-:$+		$#berk$@$1$:$2		resolve berk mail
R$+		$#local$:$1		resolve local mail

### rewriting rules for from host
S1
R$+ at $-	$1@$2			prefer "@" over "at"
R$J!$+@$-	$1@$2			arpanet mail is automatic
R$J!$-:$+	$1:$2			uucp mail is automatic

### rewriting rules for "after $g translate"
S2
R$-:$-:$+	$2:$3			delete multiple berknet hosts
R$-:$-.$+	$2:$3			delete multiple berknet hosts
R$-!$+@$-	$2@$3			delete berknet if forwarded
