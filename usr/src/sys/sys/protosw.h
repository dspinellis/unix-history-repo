/*	protosw.h	4.13	82/11/03	*/

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
 *	(*protosw[].pr_usrreq)(up, req, m, nam, opt);
 * where up is a (struct socket *), req is one of these requests,
 * m is a optional mbuf chain containing a message,
 * nam is an optional mbuf chain containing an address,
 * and opt is a pointer to a socketopt structure or nil.
 * The protocol is responsible for disposal of the mbuf chain m,
 * the caller is responsible for any space held by nam and opt.
 * A non-zero return from usrreq gives an
 * UNIX error number which should be passed to higher level software.
 */
#define	PRU_ATTACH		0	/* attach protocol to up */
#define	PRU_DETACH		1	/* detach protocol from up */
#define	PRU_BIND		2	/* bind socket to address */
#define	PRU_LISTEN		3	/* listen for connection */
#define	PRU_CONNECT		4	/* establish connection to peer */
#define	PRU_ACCEPT		5	/* accept connection from peer */
#define	PRU_DISCONNECT		6	/* disconnect from peer */
#define	PRU_SHUTDOWN		7	/* won't send any more data */
#define	PRU_RCVD		8	/* have taken data; more room now */
#define	PRU_SEND		9	/* send this data */
#define	PRU_ABORT		10	/* abort (fast DISCONNECT, DETATCH) */
#define	PRU_CONTROL		11	/* control operations on protocol */
#define	PRU_SENSE		12	/* return status into m */
#define	PRU_RCVOOB		13	/* retrieve out of band data */
#define	PRU_SENDOOB		14	/* send out of band data */
#define	PRU_SOCKADDR		15	/* fetch socket's address */
/* begin for protocols internal use */
#define	PRU_FASTTIMO		16	/* 200ms timeout */
#define	PRU_SLOWTIMO		17	/* 500ms timeout */
#define	PRU_PROTORCV		18	/* receive from below */
#define	PRU_PROTOSEND		19	/* send to below */

#define	PRU_NREQ		20

#ifdef PRUREQUESTS
char *prurequests[] = {
	"ATTACH",	"DETACH",	"BIND",		"LISTEN",
	"CONNECT",	"ACCEPT",	"DISCONNECT",	"SHUTDOWN",
	"RCVD",		"SEND",		"ABORT",	"CONTROL",
	"SENSE",	"RCVOOB",	"SENDOOB",	"SOCKADDR",
	"FASTTIMO",	"SLOWTIMO",	"PROTORCV",	"PROTOSEND",
};
#endif

/*
 * The arguments to the ctlinput routine are
 *	(*protosw[].pr_ctlinput)(cmd, arg);
 * where cmd is one of the commands below, and arg is
 * an optional argument (caddr_t).
 *
 * N.B. The IMP code, in particular, pressumes the values
 *      of some of the commands; change with extreme care.
 */
#define	PRC_IFDOWN		0	/* interface transition */
#define	PRC_ROUTEDEAD		1	/* select new route if possible */
#define	PRC_QUENCH		4	/* some said to slow down */
#define	PRC_HOSTDEAD		6	/* normally from IMP */
#define	PRC_HOSTUNREACH		7	/* ditto */
#define	PRC_UNREACH_NET		8	/* no route to network */
#define	PRC_UNREACH_HOST	9	/* no route to host */
#define	PRC_UNREACH_PROTOCOL	10	/* dst says bad protocol */
#define	PRC_UNREACH_PORT	11	/* bad port # */
#define	PRC_MSGSIZE		12	/* message size forced drop */
#define	PRC_REDIRECT_NET	13	/* net routing redirect */
#define	PRC_REDIRECT_HOST	14	/* host routing redirect */
#define	PRC_TIMXCEED_INTRANS	17	/* packet lifetime expired in transit */
#define	PRC_TIMXCEED_REASS	18	/* lifetime expired on reass q */
#define	PRC_PARAMPROB		19	/* header incorrect */

#define	PRC_NCMDS		20

#ifdef PRCREQUESTS
char	*prcrequests[] = {
	"IFDOWN",	"ROUTEDEAD",	"#2",		"#3",
	"QUNECH",	"#5",		"HOSTDEAD",	"HOSTUNREACH",
	"NET-UNREACH",	"HOST-UNREACH",	"PROTO-UNREACH","PORT-UNREACH",
	"MSGSIZE",	"NET-REDIRECT",	"HOST-REDIRECT","TX-INTRANS",
	"TX-REASS",	"PARAMPROB"
};
#endif

#ifdef KERNEL
extern	struct protosw *pffindproto(), *pffindtype();
#endif
