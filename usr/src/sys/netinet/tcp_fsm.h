/* fsm.h 1.4 81/10/29 */

/*
 * TCP FSM definitions.
 */

/*
 * States
 */
#define	TCP_NSTATES	14

#define	EFAILEC		-1		/* pseudo-state for internal use */
#define	SAME		0
#define	LISTEN		1
#define	SYN_SENT	2
#define	SYN_RCVD	3
#define	L_SYN_RCVD	4
#define	ESTAB		5
#define	FIN_W1		6
#define	FIN_W2		7
#define	TIME_WAIT	8
#define	CLOSE_WAIT	9
#define	CLOSING1	10
#define	CLOSING2	11
#define	RCV_WAIT	12
#define	CLOSED		13

/*
 * Inputs to fsm.
 */
#define	TCP_NINPUTS	10

#define	IUOPENA 	1
#define	INRECV		2
#define	IUOPENR		3
#define	IUCLOSE		4
#define	ISTIMER		5
#define	IURECV		6
#define	IUSEND		7
#define	IUABORT		8
#define	INCLEAR		9
#define	INSEND		10

/*
 * Actions
 */
#define	BAD		0
#define	LIS_CLS		1
#define	LIS_NETR	2
#define	SYR_NETR	3
#define	SYS_CLS		4
#define	SYS_NETR	5
#define	CLS_OPN		6
#define	EST_NETR	7
#define	CL2_CLW		8
#define	TIMERS		9
#define	CL1_NETR	10
#define	CL2_NETR	11
#define	CLS_RWT		12
#define	RWT_NETR	13
#define	FW1_SYR		14
#define	FW1_NETR	15
#define	FW2_NETR	16
#define	CWT_NETR	17
#define	SSS_SYN		18
#define	SSS_SND		19
#define	SSS_RCV		20
#define	CLS_NSY		21
#define	CLS_SYN		22
#define	CLS_ACT		23
#define	NOP		24
#define	CLS_ERR		25

#ifdef	KERNEL
int	acounts[14][10];
#endif

#ifdef TCPFSTAB
/* SHOULD FIGURE OUT HOW TO MAKE THIS READABLE! */
char	tcp_fstab[TCP_NSTATES][TCP_NINPUTS] = {
	{ 0, 1, 0, 4, 0, 24, 0, 0, 0, 24 },		/* CLOSED */
	{ 0, 0, 2, 0, 6, 0, 0, 0, 21, 23 },		/* LISTEN */
	{ 0, 0, 5, 0, 6, 9, 0,	0, 21, 23, },		/* SYN_SENT */
	{ 0, 0, 3, 0, 14, 9, 0,	0, 21, 23, },		/* SYN_RCVD */
	{ 0, 0, 3, 0, 14, 9, 0, 0, 21, 23, },		/* L_SYN_RCVD */
	{ 0, 0, 7, 0, 14, 9, 20, 19, 22, 23, },		/* ESTAB */
	{ 0, 0, 15, 0, 24, 9, 20, 25, 22, 23, },	/* FIN_WAIT_1 */
	{ 0, 0, 16, 0, 24, 9, 20, 24, 22, 23, },	/* FIN_WAIT_2 */
	{ 0, 0, 18, 0, 24, 9, 20, 25, 22, 23, },	/* TIME_WAIT */
	{ 0, 0, 17, 0, 8, 9, 20, 19, 22, 23, },		/* CLOSE_WAIT */
	{ 0, 0, 10, 0, 25, 9, 20, 25, 22, 23, },	/* CLOSING_1 */
	{ 0, 0, 11, 0, 25, 9, 20, 25, 22, 23, },	/* CLOSING_2 */
	{ 0, 0, 13, 0, 25, 9, 12, 25, 22, 23, },	/* RCV_WAIT */
	{ 0, 1, 0, 4, 0, 24, 0, 0, 0, 24 }		/* CLOSED */
};
#endif
#ifdef KERNEL
int	acounts[TCP_NSTATES][TCP_NINPUTS];
#endif

#ifdef TCPSTATES
char *tcpstates[] = {
	"CLOSED",	"LISTEN",	"SYN_SENT",	"SYN_RCVD",
	"L_SYN_RCVD",	"ESTAB",	"FIN_W1",	"FIN_W2",
	"TIME_WAIT",	"CLOSE_WAIT",	"CLOSING1",	"CLOSING2",
	"RCV_WAIT",	"CLOSED"
};
char *tcpinputs[] = {
	"BAD",		"UOPENA",	"NRECV",	"UOPENR",
	"UCLOSE",	"STIMER",	"URECV",	"USEND",
	"UABORT",	"NCLEAR"
};
char *tcptimers[] = { "", "INIT", "REXMT", "REXMTTL", "PERSIST", "FINACK" };
#endif

#define	TINIT		1
#define	TREXMT		2
#define	TREXMTTL	3
#define	TPERSIST	4
#define	TFINACK		5
