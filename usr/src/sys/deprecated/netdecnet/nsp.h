/*	nsp.h	1.2	82/05/15	*/

/*
 * DECnet Network Services Protocol definitions,
 * per Network Services Protocol Functional Specification,
 * Version 3.2.0, October 1980.
 */

/*
 * NSP message types
 */
#define	NSP_DATA	0x00		/* data segment message */
#define	NSP_LS		0x10		/* link service message */
#define	NSP_INTR	0x30		/* interrupt message */
#define	NSP_DATACK	0x04		/* data segment ack */
#define	NSP_OTHACK	0x14		/* other data ack */
#define	NSP_CONACK	0x24		/* connect ack */
#define	NSP_NOP		0x08		/* no op */
#define	NSP_CI		0x18		/* connect initiate */
#define	NSP_CC		0x28		/* connect confirm */
#define	NSP_DI		0x38		/* disconnect initiate */
#define	NSP_DC		0x48		/* disconnect confirm */

/* flags for data segment messages */
#define	NSP_BOM		0x20		/* beginning-of-message segment */
#define	NSP_EOM		0x40		/* end-of-message segment */

/*
 * Data segment message
 */
struct nspd {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	d_short	nsp_acknum;		/* number of ack'ed message */
	d_short	nsp_segnum;		/* this message's segment number */
};

/*
 * Interrupt message
 */
struct nspi {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	d_short	nsp_acknum;		/* number of ack'ed message */
	d_short	nsp_segnum;		/* this message's segment number */
/* optional data follows */
};

/*
 * Link Service message
 */
struct nspls {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	d_short	nsp_acknum;		/* number of ack'ed message */
	d_short	nsp_segnum;		/* this message's segment number */
	char	nsp_lsflags;		/* link service flags */
	char	nsp_fcval;		/* flow control change value */
};

#define	NSPLS_FCVALINT	0x0c		/* fcval field interpretation: */
#define	NSPLS_DATREQ	0x00		/*   data segment request */
#define	NSPLS_INTREQ	0x04		/*   interrupt request */
#define	NSPLS_FCMOD	0x03		/* flow control modification: */
#define	NSPLS_NOCHANGE	0x00		/*   no change */
#define	NSPLS_OFF	0x01		/*   do not send data */
#define	NSPLS_ON	0x02		/*   send data */

/*
 * Data or Other Data Ack
 */
struct nspack {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	d_short	nsp_acknum;		/* number of ack'ed message */
};

#define	NSPA_ACK	0x8000		/* ack flag for acknum */
#define	NSPA_NAK	0x9000		/* nak flag for acknum */
#define	NSPA_QUAL	0xf000		/* qual field for acknum */
#define	NSPA_NUM	0x0fff		/* num field for acknum */

/*
 * Connect Ack
 */
struct nspcack {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
};

/*
 * No Op message
 */
struct nspnop {
	char	nsp_msgflg;		/* message type and flags */
/* tstdata follows */
};

/*
 * Connect Initiate message
 */
struct nspci {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	char	nsp_services;		/* flow control options */
	char	nsp_info;		/* NSP version info */
	d_short	nsp_segsize;		/* requested segment size */
/* connect data follows */
};

/*
 * Connect Confirm message
 */
struct nspcc {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	char	nsp_services;		/* flow control options */
	char	nsp_info;		/* NSP version info */
	d_short	nsp_segsize;		/* requested segment size */
	char	nsp_cnt;		/* size of optional data field */
/* optional connect data follows */
};

/*
 * Disconnect Initiate message
 */
struct nspdi {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	d_short	nsp_reason;		/* reason for disconnect */
	char	nsp_dcnt;		/* size of optional data field */
/* optional disconnect data follows */
};

/*
 * Disconnect Confirm message
 */
struct nspdc {
	char	nsp_msgflg;		/* message type and flags */
	d_short	nsp_dstaddr;		/* destination address *
	d_short	nsp_srcaddr;		/* source address */
	d_short	nsp_reason;		/* disconnect reason */
};
