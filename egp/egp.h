/* egp.h */

/* EGP User Process, ISI 23-Jun-84 */

/* uses defs.h */

struct egpngh {
	struct	egpngh	  *ng_next;	/* next state table of linked list */
/*	struct	interface *ng_ifp;	/* pointer to shared net interface 
					   table */
	int	ng_state;
	int	ng_reach;	/* reachability substate */
	int	ng_flags;
	u_char	ng_status;	/* Info saved for cease retransmission */
	u_long	ng_myaddr;	/* My inet address for common net */
	u_long	ng_addr;	/* Neighbor's inet address */
	u_short	ng_sid;		/* seq.# next acq., hello or poll to send */
	u_short ng_rid;		/* seq.# last acq., hello or poll recvd */

				/* Acquire and hello info */
	int	ng_retry;	/* For states ACQUIRE_SENT and CEASE_SENT,
				  No. retries to acquire or cease neighbor.
				  For NEIGHBOR state, no. consecutive times
					neighbor is unreachable */
	int	ng_hint;	/* Send acquire, hello or cease interval 
					in seconds */
	long	ng_htime;	/* Time for next acquire, hello, or cease */
	int	ng_responses;	/* Shift register of responses for determining
				reachability, each set bit corresponds to a
				response, each zero to no response */
	int	ng_rcmd;	/* No. acq., hello and poll commands recvd
				   since lats check */

				/* NR Poll info */
	int	ng_snpoll;	/* No. sends of this NR poll id */
	int	ng_spint;	/* Send NR poll interval (in seconds) */
	long	ng_stime;	/* Time to send next NR poll */
	int	ng_runsol;	/* No. unsolicited NR msgs received for 
				   previous id */
	int	ng_rnpoll;	/* No. polls received before next poll time */
	int	ng_rpint;	/* Rcv NR poll interval (in seconds) */
	long	ng_rtime;	/* Time to rcv next NR poll */
	};

#define egpng	struct egpngh

/* States */
#define EMPTY		0
#define UNACQUIRED	1
#define ACQUIRE_SENT	2
#define NEIGHBOR	3
#define CEASE_SENT	4
#define BAD		5

/* reachability substates for state NEIGHBOR */
#define BOTH_UP	0
#define NG_DOWN	1	/* according to me my peer is unreachable */
#define ME_DOWN	2	/* according to my peer I am unreachable */

/* flags */
#define NG_BAD	1	/* change to state BAD when cease complete */

/* Basic EGP packet */
#define egpkt	struct egppkt

egpkt	{	u_char	egp_ver;	/* Version # */
		u_char	egp_type;	/* Opcode */
		u_char	egp_code;
		u_char	egp_status;
		u_short	egp_chksum;
		u_short	egp_system;	/* Autonomous system */
		u_short	egp_id;
		};

/* EGP neighbor acquisition packet */
struct egpacq	{
	egpkt	ea_pkt;
	u_short	ea_hint;	/* Hello interval in seconds */
	u_short	ea_pint;	/* NR poll interval in seconds */
	};

/* EGP NR poll packet */
struct egppoll	{
	egpkt	ep_pkt;
	u_short	ep_unused;
	u_long	ep_net;		/* Source net */
	};

/* EGP NR Message packet */
struct egpnr	{
	egpkt	en_pkt;
	u_char	en_igw;		/* No. internal gateways */
	u_char	en_egw;		/* No. external gateways */
	u_long	en_net;		/* shared net */
	};

#define NRMAXNETUNIT 9		/* maximum size per net in octets of net part
				of NR message */
/* EGP Error packet */
struct egperr	{
	egpkt	ee_pkt;
	u_short ee_rsn;
	u_char 	ee_egphd[12];	/* First 12 bytes of bad egp pkt */
};

#define EGPLEN	(sizeof(egpkt))
#define EGPVER	2

/* EGP Types */
#define EGPNR		1
#define	EGPPOLL		2
#define EGPACQ		3
#define EGPHELLO	5
#define	EGPERR		8

/* Neighbor Acquisition Codes */
#define NAREQ	0	/* Neighbor acq. request */
#define NACONF	1	/* Neighbor acq. confirmation */
#define NAREFUS	2	/* Neighbor acq. refuse */
#define NACEASE	3	/* Neighbor cease */
#define NACACK	4	/* Neighbor cease ack */

/* Neighbour Acquisition Message Status Info */
#define UNSPEC		0
#define	ACTIVE		1
#define	PASSIVE		2
#define	NORESOURCE	3
#define	ADMINPROHIB	4
#define	GODOWN		5
#define	PARAMPROB	6
#define	PROTOVIOL	7

/* Neighbor Hello Codes */
#define HELLO	0
#define HEARDU	1

/* Reachability, poll and update status */
#define INDETERMINATE	0
#define UP		1
#define DOWN		2
#define UNSOLICITED   128

/* Error reason status */
#define	EUNSPEC		0
#define EBADHEAD	1
#define	EBADDATA	2
#define ENOREACH	3
#define	EXSPOLL		4
#define ENORESPONSE	5
