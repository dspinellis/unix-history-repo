/*	protosw.h	4.4	81/11/15	*/

/*
 * Protocol switch table.
 *
 * Each protocol has a handle initializing one of these structures,
 * which is used for protocol-protocol and system-protocol communication.
 *
 * A protocol is called through the pr_init entry before any other.
 * Thereafter it is called every 100ms through the pr_fasttimo entry and
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
 *
 * The sense routine returns protocol status into the argument buffer.
 * This is used by the system in providing session-level abstractions
 * out of network level protocols, and may also be returned by socket ioctl's.
 * The amount of data returned by a sense is limited to the maxsense
 * value.  (The space for the sense is allocated by the caller, based on this.)
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
/* user-protocol hooks */
	int	(*pr_usrreq)();		/* user request: see list below */
	int	(*pr_sense)();		/* sense state of protocol */
	int	pr_maxsense;		/* max size of sense value */
/* utility hooks */
	int	(*pr_init)();		/* initialization hook */
	int	(*pr_fasttimo)();	/* fast timeout (100ms) */
	int	(*pr_slowtimo)();	/* slow timeout (1 sec) */
	int	(*pr_drain)();		/* flush any excess space possible */
};

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
#define	PRU_DISCONNECT		3	/* disconnect from peer */
#define	PRU_FLUSH		4	/* flush data in queues */
#define	PRU_SHUTDOWN		5	/* won't send any more data */
#define	PRU_RCVD		6	/* have taken data; more room now */
#define	PRU_SEND		7	/* send this data */
#define	PRU_ABORT		8	/* abort (fast DISCONNECT, DETATCH) */
#define	PRU_CONTROL		9	/* control operations on protocol */
/* begin for protocols internal use */
#define	PRU_FASTTIMO		10	/* 100ms timeout */
#define	PRU_SLOWTIMO		11	/* 500ms timeout */
#define	PRU_PROTORCV		12	/* receive from below */
#define	PRU_PROTOSEND		13	/* send to below */
/* need some stuff for splice */

#define	PRU_NREQ		14

#ifdef PRUREQUESTS
char *prurequests[] = {
	"ATTACH",	"DETACH",	"CONNECT",	"DISCONNECT",
	"FLUSH",	"SHUTDOWN",	"RCVD",		"SEND",	
	"ABORT",	"CONTROL",	"FASTTIMO",	"SLOWTIMO",
	"PROTORCV",	"PROTOSND",
};
#endif

#ifdef KERNEL
struct	protosw protosw[], *protoswLAST;
extern	struct protosw *pffindproto(), *pffindtype();
#endif
