/*
 *	DATAKIT VCS Data Structures for a DR-11C interface
 *		@(#)dkdr.h	1.1 Garage 84/03/27
 */

/*
 *  Structure for each Datakit channel
 */

struct dkchan {
	short dk_state;
	struct ifqueue dk_outq;
	int (*dk_endfcn)();
	caddr_t dk_endparm;
	int	(*dk_supfcn)();	/* who to tell of circuit supervision */
	caddr_t	dk_raddr;		/* address of received data */
	unsigned dk_rlen;		/* length of pending receive */
	short	dk_rmode;		/* possible completion modes */
	unsigned dk_xlen;		/* length of xmit */
	struct mbuf *dk_curout;		/* current buffer */
	struct dkpkbufr *dk_rq;		/* un-acked input */
	struct dkpkbufr *dk_rb;		/* un-checked input */
	short	dk_rblen;		/* len of unchecked */
	char	dk_S;			/* protocol parameters */
	char	dk_R;
	char	dk_X;
	char	dk_A;
	char	dk_C;
	char	dk_trmode;
	char	dk_rseq;
	short	dk_tail1;
	short	dk_tail2;
	int	dk_rejcnt;		/* Reject messages received */
	int	dk_srejcnt;		/* Reject messages sent */
	int	dk_ackrejcnt;		/* Acks that cause retransmit */
	int	dk_enqcnt;		/* ENQs sent */
};

/* Flags for dk_X */
#define	XM_OFF	((char) 0xff)		/* transmitter off */
#define	XM_INIT	((char) 02)		/* transmitter initialized */
#define	XM_REJ	((char) 04)		/* Sent REJ */
#define	XM_ENQ	((char) 010)		/* send ENQ next timeout */

/*
 * Packet buffers
 */

struct dkpkbufr {
	struct dkpkbufr *Pnext;
	short	Phibits;
	char Pseq;
	char Plen;
	char Pdata[16];
};

/*
 * Structure to save completion status until processed
 */

struct dkstat {
	short	k_type;		/* type of completion status */
	short	k_chan;		/* channel number */
	short	k_info1;		/* misc info */
	short	k_info2;
};
