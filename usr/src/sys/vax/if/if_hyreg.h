/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_hyreg.h	6.3 (Berkeley) %G%
 */

/*
 * Network Systems Corporation Hyperchannel interface
 *
 * supports A410 adapter interfaced via a DEC DR-11B, NSC PI-13 or PI-14
 *	(PI-14 is a PI-13 with different line drivers, software is
 *	identical to a PI-13)
 *
 * Written by Steve Glaser, Tektronix Inc., July 1982
 *
 * NOTE:
 *
 * DR11B code has not been fully checked out with 4.1a.
 * The first adapters at Tek came with DR11Bs, and the code once worked,
 * but those have been upgraded to PI-13s.
 */
#define	PI13	1	/* PI13 vs. DR11B device depandant code */
#ifndef HYLOG
#define HYLOG	1	/* enable logging of errors */
#endif

/*
 * Structure of a HYPERchannel adapter header
 */
struct	hy_hdr {
	short	hyh_ctl;	/* control */
	short	hyh_access;	/* access code */
	union {			/* to/from addresses */
		short	hyh_addr;	/* full address */
		char	hyh_baddr[2];	/* adapter/port number from address */
	} hyhu_to, hyhu_from;
#define	hyh_to		hyhu_to.hyh_addr
#define	hyh_from	hyhu_from.hyh_addr
#define	hyh_to_adapter	hyhu_to.hyh_baddr[0]
#define	hyh_to_port	hyhu_to.hyh_baddr[1]
#define	hyh_from_adapter hyhu_from.hyh_baddr[0]
#define	hyh_from_port	hyhu_from.hyh_baddr[1]
	short	hyh_param;	/* parameter (for loopback) */
	char	hyh_type;	/* record type */
	char	hyh_off;	/* offset from end of hy_hdr to ip data */
};

/*
 * Structure of a HYPERchannel message header (from software)
 */
struct	hym_data {
	short	hymd_mplen;	/* message proper length, if associated data */
};

struct	hym_hdr {
	struct	hym_data hym_d;
#define hym_mplen hym_d.hymd_mplen
	struct	hy_hdr hym_hdr;	/* hardware header, MUST BE LAST */
};

/*
 * HYPERchannel header word control bits
 */
#define	H_XTRUNKS	0x00F0	/* transmit trunks */
#define H_RTRUNKS	0x000F	/* remote trunks to transmit on for loopback */
#define H_ASSOC		0x0100	/* has associated data */
#define H_LOOPBK	0x00FF	/* loopback command */

/*
 * Structure of Statistics Record (counters)
 */
struct	hy_stat {
	u_long	hyc_msgcnt;		/* # messages transmitted */
	u_long	hyc_dbcnt;		/* # data buffers transmitted */
	u_long	hyc_tbusy;		/* # available trunks busy */
	u_long	hyc_hwret;		/* # hardware retransmits */
	u_long	hyc_crcbad;		/* # crc errors on trunk */
	u_long	hyc_mcret;		/* # microcode retransmits */
	u_long	hyc_tdabort;		/* # trunk driver aborts */
	u_char	hyc_atype[3];		/* adapter type and revision level */
	u_char	hyc_uaddr;		/* adapter unit number */
};

/*
 * Structure of the Status Record
 */
struct hy_status {
	u_char	hys_gen_status;		/* general status byte */
	u_char	hys_last_fcn;		/* last function code issued */
	u_char	hys_resp_trunk;		/* trunk response byte */
	u_char	hys_status_trunk;	/* trunk status byte */
	u_char	hys_recd_resp;		/* recieved response byte */
	u_char	hys_error;		/* error code */
	u_char	hys_caddr;		/* compressed addr of 1st msg on chain */
	u_char	hys_pad;		/* not used */
};

/*
 * Get port number from status record
 */
#define PORTNUM(p)	(((p)->hys_gen_status >> 6) & 0x03)

/*
 * The HYPERchannel driver sends and receives messages formatted:
 *
 *	+---------------------------------------+	---
 *	|					|	/|\
 *	|  HYPERchannel adapter header (hy_hdr)	|	 |
 *	|					|	 |
 *	+---------------------------------------+	 |
 *	|					|	 |
 *	|     Internet Protocol header (ip)	|    message proper
 *	|					|    (64 bytes max)
 *	+---------------------------------------+	 |
 *	|					|	 |
 *	|	TCP header + user data		|	 |
 *	|	(if it all fits here)		|	 |
 *	|					|	\|/
 *	+---------------------------------------+	---
 *
 *	+---------------------------------------+	---
 *	|					|	/|\
 *	|					|	 |
 *	|	TCP header + user data		|  associated data
 *	|					|	 |
 *	|					|	\|/
 *	+---------------------------------------+	---
 *
 * If all of the datagram will fit in the message proper (including
 * the TCP header and user data) the entire datagram is passed in
 * the message proper and the associated data feature of the HYPERchannel
 * is not used.
 *
 * The mapping from internet addresses to HYPERchannel addresses is:
 *
 *	 0       7 8      15 16                   31
 *	+---------+---------+-----------------------+
 *	| network | special | HYPERchannel address  |
 *	+---------+---------+-----------------------+
 *
 *	|<------------ internet address ----------->|
 *
 * The hyperchannel address is decoded as follows:
 *
 *       0                 7 8             13 14  15
 *	+-------------------+----------------+------+
 *	|   adapter number  |      zero      | port |
 *	+-------------------+----------------+------+
 *
 * The low 2 bits are port number (interpreted by hyperchannel hardware).
 *
 * The encoding of special bits is:
 *
 *	00	normal packet
 *
 *	01	loop this packet back to the sender at the
 *		specified adapter (ip header source/destination addresses
 *		swapped before sending, command bits added to tell the
 *		remote HYPERchannel adapter debug & performance studies]
 *		this code acts like 02 (below) if the ip destination (before
 *		any swapping) and the destination address don't match (e.g.
 *		this packet is being routed through a gateway)
 *
 *	02	loop this packet back to the sender at the
 *		specified adapter, but go through the specified adapter's
 *		IP.  This is for testing IP's store and forward mechanism.
 *
 *	other	undefined, currently treated as normal packet
 *
 */
#define MPSIZE		64	/* "Message Proper" size */
#define MAXRETRY	4

/*
 * Device registers
 */
struct	hydevice {
	short	hyd_wcr;	/* word count (negated) */
	u_short	hyd_bar;	/* bus address bits 15-0 */
	u_short	hyd_csr;	/* control and status */
	u_short	hyd_dbuf;	/* data buffer */
};

/*
 * CSR bit layout
 */
#define	S_ERROR	   0100000	/* error */
#define	S_NEX	   0040000	/* non-existent memory error */
#define	S_ATTN	   0020000	/* attn (always zero) */
#ifdef PI13
#define S_STKINTR  0010000	/* stacked interrupt */
#else
#define	S_MAINT	   0010000	/* maintenance (not used) */
#endif
#define	S_A	   0004000	/* device status A (recieve data available) */
#define	S_B	   0002000	/* device status B (normal termination) */
#define	S_C	   0001000	/* device status C (abnormal termination) */
#ifdef PI13
#define S_POWEROFF 0000400	/* power off indicator */
#else
#define	S_CYCLE	   0000400	/* cycle (not used) */
#endif
#define	S_READY	   0000200	/* ready */
#define	S_IE	   0000100	/* interrupt enable */
#define	S_XBA	   0000060	/* bus address bit bits 17 and 16 */
#define S_CLRINT   0000014	/* clear stacked interrupt */
#define	S_IATTN    0000010	/* interrupt on attention only */
#define S_WC       0000004	/* interrupt on word count == 0 only */
#define S_IATTNWC  0000000	/* interrupt on word count == 0 and attention */
#define	S_BURST	   0000002	/* burst mode DMA (not used) */
#define	S_GO	   0000001	/* go */

#define XBASHIFT	12

#define HY_CSR_BITS "\20\20ERROR\17NEX\16ATTN\15STKINTR\14RECV_DATA\13NORMAL\12ABNORMAL\11POWER\10READY\07IENABLE\06XBA17\05XBA16\04IATTN\03IWC\02BURST\01GO"

/*
 * PI13 status conditions
 */
#define	HYS_RECVDATA(x)	(((x)->hyd_csr & S_A) != 0)	/* get adapter data */
#define	HYS_NORMAL(x)	(((x)->hyd_csr & S_B) != 0)	/* done normally */
#define	HYS_ABNORMAL(x)	(((x)->hyd_csr & S_C) != 0)	/* done abnormally */
#define	HYS_ERROR(x)	(((x)->hyd_csr & S_ERROR) != 0)	/* error condition */
#define	HYS_DONE(x)	(((x)->hyd_csr & (S_ERROR|S_B|S_C)) != 0)

/*
 * Function Codes for the Hyperchannel Adapter
 * The codes are offset so they can be "or"ed into
 * the reg data buffer
 */
#define	HYF_XMITMSG	0x04	/* transmit message */
#define	HYF_XMITDATA	0x08	/* transmit associated data */
#define	HYF_XMITLSTDATA	0x0C	/* transmit last associated data */
#define	HYF_XMITLOCMSG	0x10	/* transmit local message */
#define	HYF_INPUTMSG	0x24	/* input message proper */
#define	HYF_INPUTDATA	0x28	/* input assiciated data */
#define	HYF_STATUS	0x40	/* request status */
#define	HYF_DUMPREGS	0x50	/* dump extention registers */
#define	HYF_MARKP0	0x60	/* mark down port 0 */
#define	HYF_MARKP1	0x64	/* mark down port 1 */
#define	HYF_MARKP2	0x68	/* mark down port 2 */
#define	HYF_MARKP3	0x6C	/* mark down port 3 */
#define	HYF_MP0RR	0x70	/* mark down port 0 and reroute messages */
#define	HYF_MP1RR	0x74	/* mark down port 1 and reroute messages */
#define	HYF_MP2RR	0x78	/* mark down port 2 and reroute messages */
#define	HYF_MP3RR	0x7C	/* mark down port 3 and reroute messages */
#define	HYF_RSTATS	0xA0	/* read statistics */
#define	HYF_RCSTATS	0xA4	/* read and clear statistics */
#define	HYF_SETTEST	0xC0	/* enable test operations *set test mode) */
#define	HYF_SADDR_LEN	0xC4	/* test mode: set address and length */
#define	HYF_WBUFF	0xC8	/* test mode: write buffer */
#define	HYF_RBUFF	0xCC	/* test mode: read buffer */
#define HYF_CLRADAPTER	0xE0	/* clear adapter */
#define	HYF_END_OP	0xE4	/* end operation */
#define	HYF_CLRWFMSG	0xE6	/* clear wait for mwssage */
#define	HYF_WAITFORMSG	0xE8	/* wait for message */

/*
 * Hyperchannel record types
 */
#define	HYLINK_IP	0	/* Internet Protocol Packet */

#ifdef HYLOG
#define HYL_SIZE 16*1024
struct hy_log {
	struct	hy_log *hyl_self;
	u_char	hyl_enable;		/* logging enabled? */
	u_char	hyl_onerr;		/* state to enter on error */
	u_char	*hyl_eptr;		/* &hy_log.hyl_buf[HYL_SIZE] */
	u_char	*hyl_ptr;		/* pointer into hyl_buf */
	u_char	hyl_buf[HYL_SIZE];	/* log buffer space */
};

#define HYL_NOP		0
#define HYL_UP		1	/* markup */
#define HYL_STATUS	2	/* status results (struct hy_status) */
#define HYL_STATISTICS	3	/* statistics (struct hy_stat) */
#define HYL_XMIT	4	/* packed being send (struct hym_hdr) */
#define HYL_RECV	5	/* recieved packet (short len; struct hy_hdr) */
#define HYL_CMD		6	/* cmd issued (uchar cmd, state; short count) */
#define HYL_INT		7	/* interrupt (short csr, wcr) */

#define HYL_DISABLED	0	/* logging disabled */
#define HYL_CONTINUOUS	1	/* continuous logging */
#define HYL_CAUGHT1	2	/* one buffer full captured */
#define HYL_CATCH1	3	/* one buffer full being captured */
#define HYL_CAUGHTSTATUS  4	/* one buffer of status captured */
#define HYL_CATCHSTATUS	5	/* one buffer fill of status being captured */

#ifdef  KERNEL
struct hy_log hy_log;
#endif
#endif
