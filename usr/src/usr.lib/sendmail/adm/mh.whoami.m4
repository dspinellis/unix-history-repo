divert(10)

*******************************************************
**
**	M4 Configuration information (for ucb)
**
**	This is driven from /usr/include/whoami
**
**			@(#)whoami.m4	3.3	1/2/82
**
*******************************************************

*** WHOAMI is who we are -- this drives the rest of the file
ifdef(`WHOAMI',, `define(WHOAMI, stripnl(include(`/usr/include/whoami')))')

*** Host on the ARPANET
ifdef(`ANHOST',, `define(ANHOST, Berkeley)')

*** Host on the Berkeley Network
ifdef(`BNHOST',, `define(BNHOST, CSVAX)')

*** Host on the Ether Network
ifdef(`ENHOST',, ifelse(WHOAMI, ARPAVAX,	`define(ENHOST, UCB-ARPA)',
			WHOAMI, OSCAR,		`define(ENHOST, UCB-COMET)',
			WHOAMI, KIM,		`define(ENHOST, UCB-KIM)',
			WHOAMI, INGVAX,		`define(ENHOST, UCB-INGRES)',
		       ))

*** Host on the UUCP Network
ifdef(`UNHOST',, `define(UNHOST, WHOAMI)')

*** Version of UNIX we are running and other misc. information.
ifelse(
	WHOAMI, ING70,
				`define(m4_V6)',
	WHOAMI, C70,
				nothing
	default
				`define(m4_VAX)'
				`define(m4_VFORK)'
				`define(m4_DBMLIB)'
	)

divert(0)dnl
