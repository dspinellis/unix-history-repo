
/*
 * Kernel variables for NSP
 */

typedef	short nsp_seq;

/*
 * NSP control block, ala Session Control Port,
 * p. 41-44, NSP spec.
 */
struct nspcb {
	struct	nspcb *n_next, *n_prev;	/* list of all NSP cb's */
	struct	nspcb *n_head;		/* pointer to head of list */
	struct	nspq *nq_next, *nq_prev;	/* retransmit queue */
/* NEED STUFF FOR INPUT REASSEMBLY */
	struct	socket *n_socket;	/* back pointer to socket */
	char	n_state;	/* state of the port */
	char	n_flags;	/* flags, see below */
	short	n_retrans;	/* count of message retransmissions */
	short	n_segsize;	/* transmit segment size */
	u_short	n_node;		/* remote node address */
	u_short	n_loc;		/* local link address */
	u_short	n_rem;		/* remote link address */
/* timer variables */
	u_short	nt_dat;		/* timeout for data segments */
	u_short	nt_oth;		/* timeout for other data */
	u_short	nt_con;		/* timeout for connect, disconnect */
/* sequence variables */
	nsp_seq	nn_dat;		/* number of next data segment to transmit */
	nsp_seq nn_oth;		/* number of next other data segment */
	nsp_seq	nn_high;	/* highest numbered data segment queued */
/* error control variables */
	nsp_seq	na_xmtdat;	/* number of last data segment we acked */
	nsp_seq	na_xmtoth;	/* number of last other data we acked */
	nsp_seq	na_rcvdat;	/* number of highest data segment ack rcv'ed */
/* flow control variables */
	char	nf_locdat;	/* data request count */
	char	nf_locint;	/* flow control state for receiving intr data */
	char	nf_remdat;	/* data request count from remote */
	char	nf_remint;	/* interrupt request count from remote */
/* buffers for optional data */
	u_short	nb_src;		/* source node addr for rcv CI */
	struct	mbuf *nb_con;	/* data for rcv or xmt CI */
	struct	mbuf *nb_xmt;	/* data for xmt CC, DI, Intr */
	struct	mbuf *nb_rcv;	/* data for rcv CC, DI, Intr */
};

#define	sotonspcb(so)	((struct nspcb *)(so)->so_pcb)

/* port states, p. 34-36 */
#define	NS_O		0	/* open */
#define	NS_CR		1	/* connect received */
#define	NS_DR		2	/* disconnect reject */
#define	NS_DRC		3	/* disconnect reject complete */
#define	NS_CC		4	/* connect confirm */
#define	NS_CI		5	/* connect initiate */
#define	NS_NR		6	/* no resources */
#define	NS_NC		7	/* no communication */
#define	NS_CD		8	/* connect delivered */
#define	NS_RJ		9	/* rejected */
#define	NS_RUN		10	/* running */
#define	NS_DI		11	/* disconnect initiate */
#define	NS_DIC		12	/* disconnect complete */
#define	NS_DN		13	/* disconnect notification */
#define	NS_CL		14	/* closed */
#define	NS_CN		15	/* closed notification */
#define	NS_LI		16	/* listen for connection */

/* flags */
#define	NF_DATACK	0001	/* data acknowledgement required */
#define	NF_OTHACK	0002	/* other data acknowledgement required */
#define	NF_CON		0004	/* connect data available */
#define	NF_INTAVAIL	0010	/* transmit interrupt data available */
#define	NF_OTHSENT	0020	/* other data message has been sent */
#define	NF_OTHINTR	0040	/* other data message was an interrupt msg */
#define	NF_DATOFF	0100	/* on/off switch for data flow control */

/* locint states */
/* I STILL DON'T UNDERSTAND THIS WELL ENOUGH */
#define	NFL_EMPTY	0
#define	NFL_INTR	1
#define	NFL_SEND	2
