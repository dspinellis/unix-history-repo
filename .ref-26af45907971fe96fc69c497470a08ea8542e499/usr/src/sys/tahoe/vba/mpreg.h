/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mpreg.h	7.4 (Berkeley) %G%
 */

/*
 * MPCC Asynchronous Communications Interface.
 */
#define MPINTRBASE	0xa0		/* base vector for interupts */
#define MPMAGIC		1969		/* magic number for mblok */
#define MPMAXPORT	32		/* maximum number of ports on an MPCC */

/*
 * MPCC's are capable of supporting one of a
 * the protocols listed below.  This driver
 * supports only the async terminal protocol.
 */
#define MPPROTO_UNUSED	0	/* port not in use */
#define MPPROTO_ASYNC	1	/* async protocol */
#define MPPROTO_X25	2	/* x25 protocol (unsupported) */
#define MPPROTO_BISYNC	3	/* bisync protocol (unsupported) */
#define MPPROTO_SNA  	4	/* sna protocol (unsupported) */

#define NMPPROTO	5	/* max protocols supported by MPCC */

#define MPINSET		8
#define MPOUTSET	8

/*
 * Host Interface semaphores
 */
#define MPSEMA_AVAILABLE	1
#define MPSEMA_WORK		4

/*
 * Host Interface imok values
 */
#define MPIMOK_ALIVE	0x01
#define MPIMOK_DEAD	0x80

/*
 * Host Interface Structure
 */
struct his {
	u_char	semaphore;
	u_char	imok;
	u_char	brdnum;		/* Virtual brd number for protocol */
	u_char	unused;
	struct {
		u_char	inbdone[MPMAXPORT];	/* Ports w/ inbound completed */
		u_char	outbdone[MPMAXPORT];	/* Ports w/outbound available */
		u_int	fill[2];
	} proto[NMPPROTO];
};

#define MPPORT_EOL	0xff		/* end of port list */

/*
 * Async host transmit list structure.
 */
#define MPXMIT	4	/* # of transmit ptrs/MP_WRITE event */

struct	hxmtl {
	caddr_t	dblock[MPXMIT];	/* ptrs to data blocks */
	u_short	size[MPXMIT];	/* size of each block */
};

/*
 * MPCC asynchronous protocol events.
 */
struct mpevent {
	u_char	ev_status;	/* Go Status */
	u_char	ev_cmd;		/* Optional Op-code */
	u_short	ev_opts;	/* Optional flags */
	u_short	ev_error;	/* error status returned */
	u_short	ev_flags;	/* optional event flags field */
	caddr_t	ev_params;	/* pointer to event parameters */
	union {
		struct	hxmtl *hxl;	/* pointer to host xmit list */
		u_char	*rcvblk;	/* pointer to receive block */
	} ev_un;
	u_short	ev_count;	/* # ptrs in xmit list/# receive chars  */
	u_short	ev_unused;	/* round to longword */
	u_int	ev_unused2;	/* round to size of BSC struct. GROT!! */
};

/* defines for ev_status */
#define EVSTATUS_FREE	0
#define EVSTATUS_GO	1
#define EVSTATUS_BUSY	2
#define EVSTATUS_DONE	4

/* defines for ev_cmd */
#define EVCMD_OPEN	1
#define EVCMD_CLOSE	2
#define EVCMD_RESET	3
#define EVCMD_IOCTL	4
#define EVCMD_WRITE	5
#define EVCMD_READ	6
#define EVCMD_STATUS	7
#define EVCMD_EVENT	8

/*
 * Host-MPCC interface block.
 */
struct	mblok {
	u_char	mb_status;		/* mpcc status */
	u_char	mb_ivec;		/* host interrupt vector */
	u_short	mb_magic;
	u_char	mb_diagswitch[2];	/* run diagnostics/application */
	u_char	mb_softerr;		/* soft error code */
	u_char	mb_harderr;		/* hard error code */
	struct mpdl {		/* download/config area */
		u_char	mpdl_status;	/* control/status */
		u_char	mpdl_cmd;	/* request type */
		u_short	mpdl_count;	/* size of parameter block */
		caddr_t	mpdl_data;	/* command parameters */
	} mb_dl;
	u_char	mb_hiport, mb_loport;	/* high-low mpcc port numbers */
	u_char	mb_unit;		/* mpcc unit number */
	u_char	mb_hndshk;		/* handshaking timer */
	caddr_t	mb_imokclk;		/* handshaking clock */
	u_char	mb_nointcnt;		/* no interrupt from handshake */
	u_char	mb_mpintcnt;		/* # outstanding interupts to MPCC */
	short	mb_unused;
	caddr_t	mb_mpintclk;		/* MPCC interrupt clock */
	struct	his mb_hostint;		/* To Talk with Host */
	u_char	mb_proto[MPMAXPORT];	/* per-port protocols */
	u_char	mb_intr[MPMAXPORT];	/* per-port host->mpcc int flags */
	struct	mpport {	/* per-port structure */
		u_short	mp_proto;	/* protocol of port */
		u_char	mp_on;		/* Next available entry on Host */
		u_char	mp_off;		/* Next expected 'DONE' entry on Host */
		struct	mpevent mp_recvq[MPINSET]; /* queue of events to host */
		struct	mpevent mp_sendq[MPOUTSET];/* queue of events to mpcc */
		u_char	mp_nextrcv;	/* next expected 'DONE' entry on Host */
		u_char	mp_flags;	/* host flags */
		short	mp_unused;
		caddr_t	mp_data;	/* pointer to data for port */
	} mb_port[MPMAXPORT];
};

/* status defines for mblok.status */
#define MP_DLPEND	1
#define MP_DLOPEN	2
#define MP_DLDONE	3
#define MP_OPCLOSE	4
#define MP_OPOPEN	5
#define MP_DLTIME	6
#define MP_DLERROR	(-1)

/* hard error status values loaded into mblock.herr */
#define NOHERR		0	/* no error */
#define MPBUSERR	1	/* bus error */
#define ADDRERR		2	/* address error */
#define UNDECC		3	/* undefined ecc interrupt */
#define UNDINT		4	/* undefined interrupt */
#define PWRFL		5	/* power fail occurred */
#define NOXENTRY	6	/* xdone was enterred w/o xmit entry on queue */
#define TWOFTMRS	7	/* tried to start two fast timers on one port */
#define INTQFULL	8	/* interupt queue full */
#define INTQERR		9	/* interupt queue ack error */
#define CBPERR		10	/* uncorrectable DMA parity error */
#define ACPDEAD		11	/* acap has died */
/* additional panic codes not listed */

/* soft error status values loaded into mblock.serr */
#define NOSERR	0		/* no error */
#define DMAPERR	1		/* dma parity error */
#define ECCERR	2		/* local memory ecc error */

/* Defines for flags */
#define MP_PROGRESS	1	/* Open or Close is in progress */
#define MP_IOCTL	2	/* IOCTL is in progress */
#define MP_REMBSY	4	/* remote station busy */

/*
 * Asynchronous Terminal Protocol Definitions.
 */
#define A_RCVTIM	2	/* default max tix for receive event (~20ms) */
#define ACPTMR		300	/* approx. 5 secs to wait for acap     */
#define A_MAXEVTP	3	/* maximum # of L1 or Host Events to    */
				/* process per port at one time	 */
#define A_MAXRCV	128	/* max # of chars in rcv event - enough */
				/* to hold 20ms of chars at 19.2KB      */
#define A_NUMRCV	32	/* number of rcv buffers per port       */
#define A_NUMXMT	2	/* max number of concurrent xmits/port  */
#define A_NUMEVT	32	/* number of evt bufs for status evts   */
				/* and L2 to L1 transmit evts	   */
#define WR5	5		/* SCC Write Reg 5		      */
#define TXENBL	0x08		/* mask to enable transmitter in WR 5   */
#define RTSON	0x02		/* mask to turn on RTS in wreg 5	*/
#define CHR5MSK	0x1F		/* mask for 5-bit transmit data	 */

/*
 * macro to adjust a circular buffer ptr
 *      x  = pointer or index
 *      sz = size of circular buffer
 */
#define adjptr(x,sz)	((x) = ((++(x) == (sz)) ? 0 : (x)))
#define adjptrbk(x,sz)	((x) = ((x) == 0) ? (sz) : --(x))

/*
 * Events from ASYNC Level 1 to Level 2
 */
#define RCVDTA	10	/* normal receive data available */
#define PARERR	11	/* receive data with parity error */
#define OVRNERR	12	/* receive data with overrun error */
#define OVFERR	13	/* receive data with overflow error */
#define FRAMERR	14	/* receive data with framing error */
#define ACKXMT	15	/* successful completion of transmit */
#define NORBUF	16	/* No Receive Buffers available	 */
#define NOEBUF	17	/* No Event Buffers available */
#define BRKASRT	18	/* Break condition detected */

/* defines for error conditions */
#define A_OK		0	/* No Errors */
#define A_INVEVT	1	/* Invalid Event Error */
#define A_IOCERR	2	/* Error while configuring port */
#define A_SIZERR	3	/* Error in count of data chars to xmt */
#define A_NXBERR	4	/* Transmit Incomplete due to lack of bufs */

/*
 * Modem control signal control structure.
 */
struct mdmctl {
	u_char	mc_rngdsr;	/* ring or dsr */
	u_char	mc_rts;		/* request to send */
	u_char	mc_rate;
	u_char	mc_dcd;		/* data carrier detect */
	u_char	mc_sectx;	/* secondary transmit */
	u_char	mc_cts;		/* clear to send */
	u_char	mc_secrx;	/* secondary receive */
	u_char	mc_dtr;		/* data terminal ready */
};

/* defines for modem control lines */
#define ASSERT	1		/* line asserted */
#define DROP	2		/* line dropped */
#define AUTO	3		/* auto mode enabled, rts only */

/*
 * Async parameter structure.
 */
struct asyncparam {
	u_char	ap_xon, ap_xoff;	/* xon-xoff characters */
	u_char	ap_xena;		/* xon/xoff enabled */
	u_char	ap_xany;		/* any received char enables xmitter */
	struct	mdmctl ap_modem;	/* port modem control lines */
	struct	mdmctl ap_intena;	/* modem signals which generate */
					/* status change events */
	u_char	ap_data;		/* number of data bits */
	u_char	ap_stop;		/* number of stop bits */
	u_char	ap_baud;		/* baud rate */
	u_char	ap_parity;		/* even/odd/no parity */
	u_char	ap_loop;		/* enable for local loopback */
	u_char	ap_rtimer;		/* receive timer value (msec) */
	short	ap_fill;		/* round to longword */
};

/* enable/disable signal codes */
#define MPA_ENA	1		/* condition enabled */
#define MPA_DIS	2		/* condition disabled */

/* defines for ap_data */
#define MPCHAR_5	0	/* 5 bits per character */
#define MPCHAR_6	2	/* 6 bits per character */
#define MPCHAR_7	1	/* 7 bits per character */
#define MPCHAR_8 	3	/* 8 bits per character */

/* defines for ap_stop */
#define MPSTOP_1	1	/* 1 stop bit per character */
#define MPSTOP_1_5	2	/* 1 1/2 stop bits per character */
#define MPSTOP_2	3	/* 2 stop bits per character */

/* defines for ap_baud */
#define MODEM	0
#define M0	0		/* baud rate = 0 */
#define M50	1		/* baud rate = 50 */
#define M75	2		/* baud rate = 75 */
#define M110	3		/* baud rate = 110 */
#define M134_5	4		/* baud rate = 134.5 */
#define M150	5		/* baud rate = 150 */
#define M200	6		/* baud rate = 200 */
#define M300	7		/* baud rate = 300 */
#define M600	8		/* baud rate = 600 */
#define M1200	9		/* baud rate = 1200 */
#define M1800	10		/* baud rate = 1800 */
#define M2400	11		/* baud rate = 2400 */
#define M4800	12		/* baud rate = 4800 */
#define M9600	13		/* baud rate = 9600 */
#define MEXTA	14		/* baud rate = Ext A */
#define MEXTB	15		/* baud rate = Ext B */
#define M2000	16		/* baud rate = 2000 */
#define M3600	17		/* baud rate = 3600 */
#define M7200	18		/* baud rate = 7200 */
#define M19200	19		/* baud rate = 19,200 */
#define M24000	20		/* baud rate = 24,000 */
#define M28400	21		/* baud rate = 28,400 */
#define M37800	22		/* baud rate = 37,800 */
#define M40300	23		/* baud rate = 40,300 */
#define M48000	24		/* baud rate = 48,000 */
#define M52000	25		/* baud rate = 52,000 */
#define M56800	26		/* baud rate = 56,800 */

/* defines for ap_parity */
#define MPPAR_NONE	0	/* no parity */
#define MPPAR_ODD	1	/* odd parity */
#define MPPAR_EVEN	3	/* even parity */

/* possible flags for Host MP_IOCTL Events */
#define A_CHGX		1	/* IOCTL is only chging xonxoff params */
#define A_MDMCHG	2	/* change modem control lines */
#define A_MDMGET	3	/* get current state of modem ctl lines */
#define A_CHGL1P	4	/* IOCTL is changing changing L1 params */
#define A_BRKON		5	/* set port break bit */
#define A_BRKOFF	6	/* clear port break bit */
#define A_CHGALL	7	/* IOCTL is changing xonxoff params, */
				/* pcnfg struct, & modem ctl structs */
#define A_DISABX	8	/* disable port transmitter (ctl-s) */
#define A_ENABLX	9	/* enable port transmitter (ctl-q) */

/* possible flags for Host MP_WRITE Events */
#define A_FLUSH		1	/* flush any queued transmit events */
#define A_SSTOP		2	/* transmit a port stop (xoff) char */
				/* before sending rest of event xmts */
#define A_SSTART	3	/* transmit a port start (xon) char */
				/* before sending rest of event xmts */

/* possible flags for Outbound MP_READ Events */
#define A_XOFF		1	/* transmitter stopped from by xoff char */

/* Perpos flags for modem control fields */
#define A_RNGDSR	00001
#define A_RTS		00002
#define A_RATE		00004 
#define A_DCD		00010
#define A_SECTX		00020 
#define A_CTS		00040
#define A_SECRX		00100
#define A_DTR		00200

#define DCDASRT		100	/* data carrier detect asserted */
#define DTRASRT		101	/* data terminal ready asserted */
#define RNGASRT		102	/* ring indicator asserted */
#define DSRASRT		102	/* data set ready asserted */
#define CTSASRT		103	/* clear to send asserted */
#define RTSASRT		104	/* ready to send asserted */
#define STXASRT		105	/* secondary transmit asserted */
#define SRXASRT		106	/* secondary recieve asserted */
#define RATEASRT	107	/* rate signal asserted */
#define DCDDROP		108	/* data carrier detect dropped */
#define DTRDROP		109	/* data terminal ready dropped */
#define RNGDROP		110	/* ring indicator dropped */
#define MPDSRDROP	110	/* data set ready dropped */
#define CTSDROP		111	/* clear to send dropped */
#define RTSDROP		112	/* ready to send dropped */
#define STXDROP		113	/* secondary transmit dropped */
#define SRXDROP		114	/* secondary recieve dropped */
#define RATEDROP	115	/* rate signal dropped */

/* Defines for filters and intena in portstat */
#define MDM_OFF	0
#define MDM_ON	1

/* Modem on/off flags */
#define MMOD_OFF	0
#define MMOD_ON		1

/* defintions for DL interface */

#define MPDLBUFSIZE	1024

/* mpdlioctl command defines */

struct protports {
	char protoport[MPMAXPORT];
};

struct abdcf {
	short xmtbsz;		/* transmit buffer size - should */
				/* equal # of chars in a cblock  */
};

struct bdcf {
	char loadname[NMPPROTO+1];
	char protoports[MPMAXPORT];
	char fccstimer;		/* powerfail timer */
	char fccsports;		/* ports to affect */
	char fccssoc;		/* ports which will 'switch on close' */
};


/* These ioctls are for the dlmpcc command */
#define MPIOPORTMAP		_IOW('m',1, struct protports)
#define MPIOHILO		_IOW('m',3, short)
#define MPIOENDCODE		_IO('m',4)
#define MPIOASYNCNF		_IOW('m',7, struct abdcf)
#define MPIOENDDL		_IO('m',10)
#define MPIOSTARTDL		_IO('m',11)
#define MPIORESETBOARD		_IO('m',12)

/* mpdlwrite opcode defines */

#define MPDLCMD_NORMAL	1

/* error messages printed at console , board & port # filled in later */

#define A_INVSTS	"Invalid Status Event "
#define A_INVCMD	"Invalid Event From the MPCC " 
#define A_NORBUF	"No More Available Receive Buffers "
#define A_NOEBUF	"No More Available Event Buffers "
#define A_OVRN		"Overrun Error Detected "
#define A_OVRF		"Overflow Error Detected "
#define A_NOXBUF	"No More Available Transmit Event Buffers "
#define A_XSIZE		"Transmit Data Block Size Exceeds Event Data Buffer Size "
#define A_NOFREIN	"No Available Inbound Entries to Send Close Event "
