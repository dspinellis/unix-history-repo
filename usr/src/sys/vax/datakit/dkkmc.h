/*
 *	kmc and cpu interface
 *		@(#)dkkmc.h	1.2 Garage 84/03/27
 */

/*
 *  structure for each datakit channel
 */
struct dkchan {
	short dk_state ;
	struct ifqueue dk_outq;		/* Output queue */
	struct mpacket *dk_obuf ;	/* Active mbuf output chain */
	struct mbuf *dk_pending;	/* Rest of current mbuf chain */

	struct	ifuba	dk_uba;

	int	(*dk_endfcn)() ;
	caddr_t	dk_endparm ;
	int	(*dk_supfcn)() ;
	unsigned dk_rlen;	/* length of receive buffer */
	int dk_ubmbase;	/* ubm base register used for this receive transaction */
			/* only one receive pending at a time on a channel */
};

/*
 * structure for command to kmc from cpu and
 * report to cpu from kmc
 */
struct dkkin	{
	short	k_type ;		/* cmd type or report type */
	short	k_chan ;		/* chan number */
	short	k_len ;			/* # of char */
	char	k_ctl ;			/* cntl/time */
	char	k_mode ;		/* rcv mode */
	long	k_addr ;		/* address */
} ;

/*
 * command type
 */
#define	KC_INIT		0x01	/* init: 0,0,0,0 */
#define	KC_SEND		0x02	/* send: len, 0, 0, addr */
#define	KC_RCVB		0x03	/* rcv: len, time, mode, addr */
#define	KC_CLOSE	0x04	/* close: 0, 0, 0, 0 */
#define	KC_XINIT	0x05	/* re-init xmitter: 0, 0, 0, 0 */
#define	KC_CMD		0x06	/* cmd to kmc: cmd, 0, 0, 0 */
#define	KC_FLAG		0x07	/* i/oflag: iflag, oflaghi, oflaglo, 0 */
#define	KC_SOI		0x08	/* send express: (byte2<<8)|byte1, 0, 0, 0 */
#define	KC_SCTL		0x09	/* send cntl: ctl, 0, 0, 0 */

/*
 * report type
 */
#define	KS_SEND		0x0014	/* send: 0, 0, 0, 0 */
#define	KS_RDB		0x0015	/* rcv: residue len, cntl, mode, 0 */
#define	KS_EOI		0x0016	/* rcv express: (byte2<<8)|byte1, 0, 0, 0 */
#define	KS_CNTL		0x0017	/* rcv tdk cntl: cntl, 0, 0, 0 */
#define	KS_ERR		0x0018	/* error: code, 0, 0, 0 */

/*
 * The circular buffer, cmdbuf, is used to pass command to kmc:
 * while the circular buffer statbuf is used to report status.
 * There are 8 control and status registers (csr) accessible to
 * both cpu and kmc.
 * Csr4-csr5 are iused to indicate the head and tail respectively
 * of the cmdbuf.  Likewise, csr6-csr7 for statbuf.
 * At initialization time, the cpu and kmc would agree on the beginning
 * address of both buffers and their sizes.
 */

/*
 * sub command bits for KC_CMD
 */
#define	OFLUSH	(1<<1)	/* Flush output */
#define	OSPND	(1<<2)	/* Suspend output */
#define	ORSME	(1<<3)	/* Resume output */

/*
 * KC_RCV mode
 */
#define	RCBLOCK	(1<<5)	/* return on block boundary */
#define	RCTIME	(1<<6)	/* return on time expires */
/*
 * KS_RDB mode
 */
#define	SFULL	(1<<0)	/* buffer full */
#define	SCNTL	(1<<1)	/* cntl char rcv */
#define	SBLOCK	(1<<5)	/* block boundary */
#define	STIME	(1<<6)	/* time limit expired */
#define	SABORT	(1<<3)	/* rcv aborted */
/*
 * KC_SEND mode
 */
#define	SBOT	0	/* End blocks with BOT */
#define	SBOTM	0x80	/* End blocks with BOTM */

/*
 * KS_ERR codes
*/
#define	E_SW		0x00	/* dispatcher switch */
#define	E_BUS		0x01	/* Unibus error */
#define	E_IPANIC	0x02	/* input routine panic */
#define	E_CMD		0x03	/* command unknown */
#define	E_NOQB		0x04	/* run out of queue or buffer */
#define	E_DUP		0x05	/* duplicate SEND */
#define	E_ODKOVF	0x06	/* output routine panic */
#define	E_UMETA		0x07	/* un-recognized cntl char */
#define	E_SYS1		0x0021	/* system error 1 (041) */
#define	E_SYS2		0x0022	/* system error 2 (042) */
