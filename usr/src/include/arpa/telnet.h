/*	telnet.h	4.1	82/02/28	*/

/*
 * Definitions for the TELNET protocol.
 */
#define	IAC	255		/* interpret as command: */
#define	DONT	254		/* you are not to use option */
#define	DO	253		/* please, you use option */
#define	WONT	252		/* I won't use option */
#define	WILL	251		/* I will use option */
#define	SB	250		/* interpret as subnegotiation */
#define	GA	249		/* you may reverse the line */
#define	EL	248		/* erase the current line */
#define	EC	247		/* erase the current character */
#define	AYT	246		/* are you there */
#define	AO	245		/* abort output--but let prog finish */
#define	IP	244		/* interrupt process--permanently */
#define	BREAK	243		/* break */
#define	DM	242		/* data mark--for connect. cleaning */
#define	NOP	241		/* nop */
#define	SE	240		/* end sub negotiation */

#define SYNCH	242		/* for telfunc calls */

/* telnet options */

#define TELOPT_BINARY	0	/* 8-bit data path */
#define TELOPT_ECHO	1	/* echo */
#define	TELOPT_RCP	2
#define	TELOPT_SGA	3
#define	TELOPT_NAME	4
#define	TELOPT_STATUS	5
#define	TELOPT_TM	6
#define	TELOPT_RCTE	7
#define TELOPT_NAOL 	8
#define TELOPT_NAOP 	9
#define TELOPT_NAOCRD	10
#define TELOPT_NAOHTS	11
#define TELOPT_NAOHTD	12
#define TELOPT_NAOFFD	13
#define TELOPT_NAOVTS	14
#define TELOPT_NAOVTD	15
#define TELOPT_NAOLFD	16
#define TELOPT_XASCII	17
#define TELOPT_EXOPL	255

#ifdef TELCMDS
char *telcmds[] = {
	"SE", "NOP", "DMARK", "BRK", "IP", "AO", "AYT", "EC",
	"EL", "GA", "SB", "WILL", "WONT", "DO", "DONT", "IAC",
};
#endif

#ifdef TELOPTS
char *telopts[] = {
	"BINARY", "ECHO", "RCP", "SUPPRESS GO AHEAD", "NAME",
	"STATUS", "TIMING MARK", "RCTE", "NAOL", "NAOP",
	"NAOCRD", "NAOHTS", "NAOHTD", "NAOFFD", "NAOVTS",
	"NAOVTD", "NAOLFD", "EXTEND ASCII",
};
#endif
