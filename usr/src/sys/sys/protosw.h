/*	protosw.h	4.1	81/11/07	*/

/*
 * Protocol switch table.
 *
 * Each protocol has a handle initializing one of these structures,
 * which is used for protocol-protocol and system-protocol communication.
 *
 * Protocols pass data between themselves as chains of mbufs using
 * the pr_input and pr_output hooks.  Pr_input passes data up (towards
 * UNIX) and pr_output passes it down (towards the imps).
 * The protocol is responsible for the space occupied by any of its
 * arguments and must dispose of the space when it is finished with it.
 *
 * The advise entry is used by lower level protocols to inform
 * a higher level protocol of errors and routing advisories and the like.
 * Fasttimo is called every 100ms and is typically used to instantaeously
 * delay sending ACK's while slowtimo is called every 500ms and is used
 * for longer term cleanup.
 *
 * The drain routine is called if the system is low on buffer space, and
 * should throw away non-cricital data.  The userreq routine interfaces
 * protocols to the system and is described below.  The sense routine
 * returns protocol status into the argument buffer.  This is used by
 * the system in providing session-level abstractions out of network
 * level protocols, and may also be returned by socket ioctl's.
 * The amount of data returned by a sense is limited to the maxsense
 * value.  (The space for the sense is allocated by the caller, based on this.)
 */
struct protosw {
	short	pr_type;		/* socket type used for */
	short	pr_family;		/* protocol family */
	short	pr_protocol;		/* protocol number */
	int	pr_flags;		/* see below */
	int	(*pr_input)();		/* input to protocol (from below) */
	int	(*pr_output)();		/* output to protocol (from above) */
	int	(*pr_advise)();		/* advise about error condition */
	int	(*pr_fasttimo)();	/* fast timeout (100ms) */
	int	(*pr_slowtimo)();	/* slow timeout (1 sec) */
	int	(*pr_drain)();		/* flush any excess space possible */
	int	(*pr_usrreq)();		/* user request: see list below */
	int	(*pr_sense)();		/* sense state of protocol */
	int	pr_maxsize;		/* max size of sense value */
};

/*
 * Values for pr_flags
 */
#define	PR_ATOMIC	0x01		/* exchange atomic messages only */
#define	PR_ADDR		0x02		/* addresses given with messages */
/* in the current implementation, PR_ADDR needs PR_ATOMIC to work */

/*
 * The arguments to usrreq are:
 *	(*protosw[].pr_usrreq)(up, req, m, addr);
 * where up is a (struct socket *), req is one of these requests,
 * m is a optional mbuf chain, and addr is an optional meta-internetwork
 * address representation.  The protocol is responsible for
 * disposal of the mbuf chain.  A non-zero return from usrreq gives an
 * UNIX error number which should be passed to higher level software.
 */
#define	PRU_ATTACH	0	/* attach protocol to up */
#define	PRU_DETACH	1	/* detach protocol to up */
#define	PRU_CONNECT	2	/* establish connection to peer */
#define	PRU_DISCONNECT	3	/* disconnect from peer */
/* for ISCONN and ISDISCONN a 0 return means yes */
#define	PRU_ISCONN	4	/* is connection to peer complete? */
#define	PRU_ISDISCONN	5	/* is disconnection from peer complete? */
#define	PRU_RCVD	6	/* have taken data; more room now */
#define	PRU_SEND	7	/* send this data */
#define	PRU_ABORT	8	/* abort (fast DISCONNECT, DETATCH) */
#define	PRU_CLEAR	9	/* network went down: clean up */
#define	PRU_CONTROL	10	/* control operations on protocol */
#define	PRU_FASTTIMO	11	/* for protocol's use only: fast timeout */
#define	PRU_SLOWTIMO	12	/* for protocol's use only: slow timeout */
/* need some stuff for splice */

#ifdef KERNEL
struct protosw protosw[];
#endif
