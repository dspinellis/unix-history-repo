/*
 $Log:	rdp.h,v $
 * Revision 2.13  85/06/18  14:33:36  walsh
 * added extern decl.
 * 
 * Revision 2.12  84/11/29  12:50:15  walsh
 * changed references to currentrtt into references to srtt, a better
 * and less misleading mnemonic.
 * 
 * Revision 2.11  84/11/15  09:55:15  walsh
 * redid how we deal with compiler padding in the RDP header structure.
 * 
 * Revision 2.10  84/11/08  16:09:21  walsh
 * Added code to gather statistics on RDP traffic.  This makes the RDPCB
 * too big unles you make mbufs 512 bytes large.  RDP_CS should be turned off
 * unless you do.
 * 
 * Revision 2.9  84/11/06  15:24:02  walsh
 * *** empty log message ***
 * 
 * Revision 2.8  84/11/06  14:29:44  walsh
 * intorduced RDP_HLSHIFT
 * 
 * Revision 2.7  84/11/05  12:42:04  walsh
 * Set things up so can debug RDP connections just like can debug TCP
 * connections.
 * 
 * Revision 2.6  84/11/05  11:14:03  walsh
 * *** empty log message ***
 * 
 * Revision 2.5  84/11/05  11:03:55  walsh
 * comment and adjust number for rdp_iss in a mathematically correct way
 * as a result of benchmarks (cf. operationally correct).
 * 
 * Revision 2.4  84/11/02  18:21:13  walsh
 * Protocol specifiers want NULL message to have own sequence number in
 * case of slow (t>NULL msg timeout) packets.  I don't see this as a problem,
 * and even if happened (dubious) would only delay discovery, but I
 * didn't win this one.  Initially not designed for this, but fixes are
 * in almost neatly.
 * 
 * Revision 2.3  84/11/02  15:26:42  walsh
 * Allow for RDP header fields not on natural boundries.  (Protocol
 * specifiers say will be part of next version in 6-12 months).
 * Until then, there goes the speed...  Yucho modifications.
 * 
 * Revision 2.2  84/11/02  13:12:29  walsh
 * use rdp typedefs for packet header fields to reduce lint errors.
 * 
 * Revision 2.1  84/11/02  10:10:49  walsh
 * Fixed to include RCS comments in checked out source.
 * 

 * description:
 * Reliable Datagram Protocol definitions.
 * 
 * revision 1.10        
 * date: 84/07/25 09:45:36;  author: walsh;  state: Exp;  lines added/del: 1/1
 * RDP finally has an official protocol number assigned by Postel.
 * 
 * revision 1.9        
 * date: 84/07/19 10:20:37;  author: walsh;  state: Exp;  lines added/del: 1/31
 * Organized macros and classified their definitions in rdp_macros.h.
 * 
 * revision 1.8        
 * date: 84/07/18 18:49:39;  author: walsh;  state: Exp;  lines added/del: 7/1
 * Added provision for sending of NULL messages.  These are sent on an idle
 * connection to determine that the other side still exists.
 * 
 * revision 1.7        
 * date: 84/07/18 13:51:54;  author: walsh;  state: Exp;  lines added/del: 1/0
 * constant RDP_MAXTIMERVAL to go with type rdptimerval.
 * 
 * revision 1.6        
 * date: 84/07/18 13:27:06;  author: walsh;  state: Exp;  lines added/del: 11/4
 * Bouncing datagrams off goonhilly-echo eventually causes RTTL timeout.  So,
 * have re-worked:
 * 	1.  rxtime = 1.5 srtt, not factor of 2 used before
 * 	2.  max rxtime now 20 sec, not 30 seconds
 * 	3.  provisions for user definition of RTTL period.
 * 
 * revision 1.5        
 * date: 84/07/12 13:44:12;  author: walsh;  state: Exp;  lines added/del: 26/19
 * Rather than in-line stuffing of IP/RDP headers, at least half of which are
 * constant, copy headers in from a template of what the headers are like.  The
 * bcopy() call is turned into a movc3 instruction on the VAX by a sed script
 * run over the assembler output of the C compiler.  Marginal speed-up.
 * 
 * revision 1.4        
 * date: 84/07/10 14:42:34;  author: walsh;  state: Exp;  lines added/del: 20/23
 * simplified check for if debugging on in RDP_ACTION, since rdpcb, inpcb,
 * and socket structure all last equally as long.
 * 
 * revision 1.3        
 * date: 84/07/09 14:23:00;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Added ACK-delay timer.
 * 
 * revision 1.2        
 * date: 84/07/06 09:46:45;  author: walsh;  state: Exp;  lines added/del: 36/1
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:16:27;  author: walsh;  state: Exp;  
 * Initial revision
 * 
 */


/*
 * Here's how I've tried to name things to keep them consistent.
 * For RDP_?..., the ? means:
 *
 *	f	flag (in packet header)
 *	i	input event
 *	o	option (in syn packet header)
 *	p	port number
 *	s	state (of connection in rdpcb)
 *	t	timer
 *	tv	timer value
 */

typedef char	boolean;
typedef u_char	rdpstate;
typedef u_long	rdpsequence;	/* sequence # or ack # */
typedef u_long	rdpchecksum;
typedef u_char	rdptimerval;
#define RDP_MAXTIMERVAL	255
typedef u_char	rdpportnum;

typedef struct inpcb	INPCB;	/* belongs in in_pcb.h */
typedef struct mbuf	MBUF;	/* belongs in mbuf.h */


typedef struct rdphdr {
	char		rh_ver:2,	/* version of packet header */
	        	rh_flags:6;
	u_char		rh_hdrlen;	/* in units of 2 bytes */
	rdpportnum	rh_sport;	/* source port */
	rdpportnum	rh_dport;	/* destination port */
	u_short		rh_dlen;	/* amount of data following the header */
	rdpsequence	rh_seqno;
	rdpsequence	rh_ackno;	/* valid iff ACK set */
	rdpchecksum	rh_cksum;
} RDPHDR;

#define RDP_VERSION	1		/* rh_ver */

#define RDP_fNULL	0x02		/* rh_flags */
#define RDP_fRST	0x04
#define RDP_fEACK	0x08
#define RDP_fACK	0x10
#define RDP_fSYN	0x20

#define RDP_HLSHIFT 1	/* Header Length SHIFT */
#define hdrlen(x) ((x)->rh_hdrlen << RDP_HLSHIFT)

/*
 * RDP port numbers 1-63 for servers, 64-255 assigned to clients
 */

#define RDP_pANY		  0
#define RDP_RESERVED		 63
#define RDP_USERRESERVED	 63
#define RDP_MAXPORT		255

/*
 * Due to the compiler aligning header fields on natural boundries,
 * the rdp header is 18 bytes on the network, but sizeof(RDPHDR) = 20
 * So, skip sizeof and define macros to access fields after the gap.
 */
#define RDPHDRSZ	18	/* on the network */

#define RDP_SEQNO(pkt)		(* ((u_long *) (&((char *) (pkt))[6])))
#define RDP_ACKNO(pkt)		(* ((u_long *) (&((char *) (pkt))[10])))
#define RDP_CKSUM(pkt)		(* ((u_long *) (&((char *) (pkt))[14])))
#define RDP_OPT(pkt, type)	((type) (&((char *) (pkt))[RDPHDRSZ]))

/*
 * In a SYN packet, this will immediately follow the rdphdr
 */
typedef struct synoptions {
	short	rh_nbuf;	/* # dgrams he can buffer */
	u_short	rh_maxlen;	/* max length of a dgram */
	short	rh_options;
#define RDP_oSEQUENTIAL	1
} SYNOPTIONS;

/*
 * For an established connection, a variable length array of these
 * may immediately follow the rdphdr
 */
typedef struct eackoptions {
	rdpsequence	rh_eackno;
} EACKOPTIONS;



/*
 * RDP connection states
 */
#define RDP_sSAME	0	/* no state transition for this input to fsm */
#define RDP_sUNOPENED	1	/* after socket(2), before listen or connect */
#define RDP_sLISTEN	2	/* after listen(2) */
#define RDP_sSYNSENT	3	/* after connect(2) */
#define RDP_sLSYNRCVD	4	/* child socket after SYN gets to LISTENer */
#define RDP_sSYNRCVD	5	/* after SYN gets to SYNSENT */
#define RDP_sESTAB	6	/* after get both SYN and ACK */
#define RDP_sCLOSEWAIT	7	/* after send or receive RST */
#define RDP_sCLOSED	8	/* after CLOSEWAIT timeout */

#define RDP_NSTATES	9

/*
 * Inputs that (possibly) cause state transition
 */
#define RDP_iCONNECT	0	/* user connect(2) request == active open */
#define RDP_iLISTEN	1	/* user listen(2) request == passive open */
#define RDP_iNETR	2	/* network reception of packet */
#define RDP_iUCLOSE	3	/* user close(2) request */
#define RDP_iTIMER	4	/* a timer went off */
#define RDP_iRCV	5	/* user has picked up a packet */
#define RDP_iSEND	6	/* user send request */
#define RDP_NINPUTS	7



/*
 * rq_maxqlen = MIN(desired, RDP_MAXDGRAMS)
 */
#define RDP_MAXDGRAMS	40

/*
 * In rq_msgs, pointers follow:
 *	sendq:: NULL x PRU_SEND ->
 *		ptr x (E)ACK ->
 *		RDP_DELIVERED x move front -> NULL
 *
 *	rcvq::	NULL x net reception ->
 *		ptr x pass to user ->
 *		RDP_DELIVERED x (PRU_RECV + move front) -> NULL
 *
 *		on last transition, we also (E)ACK the packet.
 */
#define RDP_DELIVERED	((struct mbuf *) (-1))
#define RDP_NULLMSG	((struct mbuf *) (-2))

/*
 * rq_msgs points into an mbuf that we use for an array of pointers to
 * mbuf chains.  On input, each mbuf chain holds an RDP packet (header and
 * data).  On output, each mbuf chain holds the data portion of the packet
 * in case it is needed for re-transmission.
 *			+---------------+
 *	rq_msgs -->	|		|
 *			|   o-----------|---> mbuf chain (== packet or data)
 *			| RDP_DELIVERED	|
 *			| RDP_NULLMSG	|
 *			|		|
 *			+---------------+
 *			array in mbuf
 */
typedef struct rdp_msgq {
	int		 rq_maxqlen;	/* 1...RDP_MAXDGRAMS inclusive */
	int		 rq_maxiplen;	/* max IP length of dgram can put on q*/
	int		 rq_front;	/* 0...(rq_maxqlen-1) inclusive */
	rdpsequence	 rq_baseseq;	/* RDP seq # of rq_msgs[rq_front] */
	MBUF	       **rq_msgs;	/* -> into mbuf holding array of ptrs*/
} RDP_MSGQ;


typedef struct rdpcb {
	struct inpcb	*r_inpcb;

	rdpstate	 r_state;	/* state of connection */
	rdpsequence	 r_iss;		/* initial sequence # sent (in SYN) */
	rdpsequence	 r_irs;		/* initial sequence # rcvd (in SYN) */
	rdpsequence	 r_sndnxt;	/* seq # for next datagram we send */

	struct rdp_msgq	r_sendq, r_rcvq;

#define r_hisnbuf r_sendq.rq_maxqlen	/* # RDP messages he can buffer */
#define r_hismaxlen r_sendq.rq_maxiplen /* biggest IP datagram he'll take */
#define r_snduna r_sendq.rq_baseseq	/* seq # of oldest unacked dgram sent */
#define r_ournbuf r_rcvq.rq_maxqlen	/* # RDP messages we can buffer */
#define r_ourmaxlen r_rcvq.rq_maxiplen  /* biggest RDP data length we'll take */

	boolean		 r_synrcvd;	/* have we rcvd his SYN? */
	boolean		 r_synacked;	/* has he ACKed our SYN? */
	boolean		 r_usrclosed;	/* has user process close(2)ed yet? */
	boolean		 r_sendrst;	/* set reset in outgoing packet */
	boolean		 r_sendnull;	/* set null in outgoing packet */
	boolean		 r_sequential;	/* sequential delivery? */
	boolean		 r_rttiming;	/* are we measuring rtt? */

	rdptimerval	 r_closewait;	/* # 0.5 sec units before destroy *cb */
#define RDP_tvCLOSEWAIT	120		/* default idea of CLOSEWAIT timer */
	rdptimerval	 r_rttl;	/* error if dgram unacked in this time*/
#define RDP_tvRTTL	120		/* default idea of RTTL timer */
	rdptimerval	 r_tvnull;	/* for testing connection existence */
#define RDP_tvNULL	240		/* default idea time to 1st NULL */
	char		 r_nullsent;	/* # successive null messages sent */
#define RDP_MAXNULL	  5

	/*
	 * For now, each retransmission of a packet will take the same
	 * amount of time.
	 */
	rdptimerval	 r_rxmitime;	/* current idea of re-xmission time */
#define RDP_tvRXMAX	40		/* max value of re-xmit timer (20 sec)*/
#define RDP_tvRXMIN	 4		/* min value of re-xmit timer ( 2 sec)*/
	/*
	 * (3 * (RDP_tvRXMIN = 4)) / 2 = 6
	 * So, allows AT LEAST one second of variance from srtt until
	 * hits RDP_tvRXMAX ceiling.
	 */
#define update_rxmitime(r) \
  (r)->r_rxmitime = MAX(RDP_tvRXMIN, \
			MIN(RDP_tvRXMAX, (3 * (r)->r_srtt) / 2));

	/*
	 * if we're measuring the round trip time, (r_rttiming == TRUE),
	 * then r_rtt counts time til get ack of dgram # r_rttimed.
	 * r_rtt starts at 0 when send packet # r_rttimed, and is incremented
	 * each 0.5 second.  r_srtt is updated when r_rttimed is acked.
	 * At that time, r_rxmitime should also be updated.  srtt represents
	 * a weighted average of the recent round trip times.
	 */
	rdpsequence	 r_rttimed;	/* seq # of dgram finding rtt for */
	rdptimerval	 r_rtt;		/* round trip time (in 0.5 sec units)*/
	rdptimerval	 r_srtt;	/* smoothed round trip time */
#define ALPHA	4
#define BETA	1
#ifdef RDP_CS
#define update_rttestimate(r) \
  { \
  (r)->r_srtt = (ALPHA*(r)->r_srtt + BETA*(r)->r_rtt) / (ALPHA+BETA) \
  (r)->r_nrtt ++;					\
  (r)->r_totalrtt += (r)->r_rtt;			\
  if ((r)->r_nrtt == 1)					\
	(r)->r_minrtt = (r)->r_maxrtt = (r)->r_rtt;	\
  else if ((r)->r_rtt < (r)->r_minrtt)			\
	(r)->r_minrtt = (r)->r_rtt;			\
  else if ((r)->r_rtt > (r)->r_maxrtt)			\
	(r)->r_maxrtt = (r)->r_rtt;			\
  }
#else
#define update_rttestimate(r) \
  (r)->r_srtt = (ALPHA*(r)->r_srtt + BETA*(r)->r_rtt) / (ALPHA+BETA)
#endif

	/*
	 * if we have at least one packet being timed for re-transmission,
	 * then we have a "retransmit took too long" timer also set.  One such
	 * timer suffices.  This timer is associated with rxtimers[rttlindex]
	 * and r_sendq.rq_msgs[rttlindex]
	 */
	int		 r_rttlindex;

#define RDP_tCLOSEWAIT	0
#define RDP_tRTTL	1	/* retransmit took too long (not in spec) */
#define RDP_tRXMIT	2	/* if set, check rxtimers */
#define RDP_tACKDELAY	3
#define RDP_tNULL	4
#define RDP_NTIMERS	5
	rdptimerval	 r_timers[RDP_NTIMERS];

	/*
	 * The re-transmission timer array is parallel to r_sendq.rq_msgs
	 */
	rdptimerval	 r_rxtimers[RDP_MAXDGRAMS]; /* send retransmit timers */
#define RDP_tvRXCHECK	1	/* check per-pkt rxmit timers every 0.5 sec   */

	/*
	 * and for a (minor) speedup, just byte copy the constant header fields
	 */
#define RDP_TEMPLSIZE	(sizeof(struct ip) + RDPHDRSZ)
	char		 r_template[RDP_TEMPLSIZE];

#ifdef RDP_CS
	rdptimerval	r_minrtt;	/* minimum rtt observed */
	rdptimerval	r_maxrtt;	/* maximum rtt observed */
	int		r_totalrtt;	/* total of all rtt packets */
	int		r_nrtt;		/* # rtt packets measured */

	u_long		r_entered[RDP_NSTATES]; /* .001 sec */

	struct {
		int	r_total;
		int	r_nullpkts;
		int	r_synpkts;
		int	r_rstpkts;
		int	r_retrans;
		int	r_nbytes;	/* to/from user */
	} r_sent, r_rcvd;
#endif
} RDPCB;

#define rdpcbtoso(r)	((r)->r_inpcb->inp_socket)

/*
 * RDP desires control over the IP length.  We really only have good
 * control on the RDP data length in the UNIX socket code.  Use the following
 * as the difference between the two.
 *
 * Allow how much space for eacks?
 * (Don't care on input.  Can drop eack options on output.)
 */
#define HDRSLOP (RDPHDRSZ + sizeof(struct ip))

/*
 * Active opens (connect) and children of listener can time out via RDP_tRTTL.
 * Is a timeout for passive opens (LISTEN state) desireable?  Prob not
 * since user can always use alarm(2) system call.
 */



typedef struct r_debug {
	u_long		rd_iptime;	/* 0.001 second units */
	int		rd_input;
	rdpstate	rd_newstate;
	RDPCB		rd_rdpcb;

	int		rd_timer;	/* iff input == RDP_iTIMER */
	struct ip	rd_iphdr;	/* iff input == RDP_iNETR */
	RDPHDR		rd_rdphdr;	/* iff input == RDP_iNETR */
} R_DEBUG;

#define RCDBLEN ((CLBYTES/sizeof(R_DEBUG)) * sizeof(R_DEBUG))
#define RDBLEN	((MLEN   /sizeof(R_DEBUG)) * sizeof(R_DEBUG))

#define inptordpcb(i)	((RDPCB *) ((i)->inp_ppcb))

struct rdp_stat {
	struct in_stat r_in;
#define r_total		r_in.in_total
#define r_badsum	r_in.in_badsum
#define r_tooshort	r_in.in_tooshort
#define r_drops		r_in.in_drops
};


#ifdef KERNEL
/*
 * Each host chooses the starting point of the packet numbering for a
 * connection so that datagrams from different incarnations of a
 * connection have no sequence numbers in common.  SYN packets are used
 * by each side to make the other aware of the starting point.  If we
 * can send N packets per slow timeout, then if we update rdp_iss
 * by RDP_ISSINCR > N every slow timeout (one connection bangs away
 * all timeout period) and by RDP_ISSINCR every time we make a connection
 * (many incarnations of same connection per timeout period), then we're
 * o.k.
 */
extern rdpsequence rdp_iss;

/* vax can send 180 packets/second */
#define RDP_ISSINCR ((200/PR_SLOWHZ) +1)

extern struct dfilter rdp_dfilter;
extern struct inpcb rdp;
extern struct rdp_stat rdpstat;
extern rdp_pcbdisconnect();
extern struct mbuf *rdp_qremove();
extern rdpchecksum rdp_cksum();
extern char *rdp_conn_used();

#define rdp_action(input, rdpcb, arg) rdpaction(input, rdpcb, (int) arg)


/*
 * RDP finite state machine
 */
#ifdef RDPDEBUG
extern char	*rdpstates[RDP_NSTATES];	/* rdpstate -> string */
extern char	*rdpinputs[RDP_NINPUTS];
extern char	*rdptimers[RDP_NTIMERS];
#endif
extern int	(*rdp_action_table[RDP_NSTATES][RDP_NINPUTS])();
#endif
