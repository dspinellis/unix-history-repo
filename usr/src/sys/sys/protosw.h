/*	protosw.h	4.10	82/04/10	*/

/*
 * Protocol switch table.
 *
 * Each protocol has a handle initializing one of these structures,
 * which is used for protocol-protocol and system-protocol communication.
 *
 * A protocol is called through the pr_init entry before any other.
 * Thereafter it is called every 200ms through the pr_fasttimo entry and
 * every 500ms through the pr_slowtimo for timer based actions.
 * The system will call the pr_drain entry if it is low on space and
 * this should throw away any non-critical data.
 *
 * Protocols pass data between themselves as chains of mbufs using
 * the pr_input and pr_output hooks.  Pr_input passes data up (towards
 * UNIX) and pr_output passes it down (towards the imps); control
 * information passes up and down on pr_ctlinput and pr_ctloutput.
 * The protocol is responsible for the space occupied by any the
 * arguments to these entries and must dispose it.
 *
 * The userreq routine interfaces protocols to the system and is
 * described below.
 */
struct protosw {
	short	pr_type;		/* socket type used for */
	short	pr_family;		/* protocol family */
	short	pr_protocol;		/* protocol number */
	short	pr_flags;		/* see below */
/* protocol-protocol hooks */
	int	(*pr_input)();		/* input to protocol (from below) */
	int	(*pr_output)();		/* output to protocol (from above) */
	int	(*pr_ctlinput)();	/* control input (from below) */
	int	(*pr_ctloutput)();	/* control output (from above) */
/* user-protocol hook */
	int	(*pr_usrreq)();		/* user request: see list below */
/* utility hooks */
	int	(*pr_init)();		/* initialization hook */
	int	(*pr_fasttimo)();	/* fast timeout (200ms) */
	int	(*pr_slowtimo)();	/* slow timeout (500ms) */
	int	(*pr_drain)();		/* flush any excess space possible */
};

#define	PR_SLOWHZ	2		/* 2 slow timeouts per second */
#define	PR_FASTHZ	5		/* 5 fast timeouts per second */

/*
 * Values for pr_flags
 */
#define	PR_ATOMIC	0x01		/* exchange atomic messages only */
#define	PR_ADDR		0x02		/* addresses given with messages */
/* in the current implementation, PR_ADDR needs PR_ATOMIC to work */
#define	PR_CONNREQUIRED	0x04		/* connection required by protocol */
#define	PR_WANTRCVD	0x08		/* want PRU_RCVD calls */

/*
 * The arguments to usrreq are:
 *	(*protosw[].pr_usrreq)(up, req, m, addr);
 * where up is a (struct socket *), req is one of these requests,
 * m is a optional mbuf chain, and addr is an optional meta-internetwork
 * address representation.  The protocol is responsible for
 * disposal of the mbuf chain.  A non-zero return from usrreq gives an
 * UNIX error number which should be passed to higher level software.
 */
#define	PRU_ATTACH		0	/* attach protocol to up */
#define	PRU_DETACH		1	/* detach protocol from up */
#define	PRU_CONNECT		2	/* establish connection to peer */
#define	PRU_ACCEPT		3	/* accept connection from peer */
#define	PRU_DISCONNECT		4	/* disconnect from peer */
#define	PRU_SHUTDOWN		5	/* won't send any more data */
#define	PRU_RCVD		6	/* have taken data; more room now */
#define	PRU_SEND		7	/* send this data */
#define	PRU_ABORT		8	/* abort (fast DISCONNECT, DETATCH) */
#define	PRU_CONTROL		9	/* control operations on protocol */
#define	PRU_SENSE		10	/* return status into m */
#define	PRU_RCVOOB		11	/* retrieve out of band data */
#define	PRU_SENDOOB		12	/* send out of band data */
#define	PRU_SOCKADDR		13	/* fetch socket's address */
/* begin for protocols internal use */
#define	PRU_FASTTIMO		14	/* 200ms timeout */
#define	PRU_SLOWTIMO		15	/* 500ms timeout */
#define	PRU_PROTORCV		16	/* receive from below */
#define	PRU_PROTOSEND		17	/* send to below */

#define	PRU_NREQ		18

#ifdef PRUREQUESTS
char *prurequests[] = {
	"ATTACH",	"DETACH",	"CONNECT",	"ACCEPT",
	"DISCONNECT",	"SHUTDOWN",	"RCVD",		"SEND",	
	"ABORT",	"CONTROL",	"SENSE",	"RCVOOB",
	"SENDOOB",	"SOCKADDR",	"FASTTIMO",	"SLOWTIMO",
	"PROTORCV",	"PROTOSEND",
};
#endif

#ifdef KERNEL
struct	protosw protosw[], *protoswLAST;
extern	struct protosw *pffindproto(), *pffindtype();
#endif
