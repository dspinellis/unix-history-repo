/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_hy.h	7.2 (Berkeley) %G%
 */

/*
 * 4.2 BSD Unix Kernel - Vax Network Interface Support
 *
 * $Header: if_hy.h,v 10.0 84/06/30 19:51:21 steveg Stable $
 * $Locker:  $
 *
 * Modifications from Berkeley 4.2 BSD
 * Copyright (c) 1983, Tektronix Inc.
 * All Rights Reserved
 *
 *
 * $Log:	if_hy.h,v $
 *	Revision 10.0  84/06/30  19:51:21  steveg
 *	Big Build
 *	
 *	Revision 3.13  84/05/30  19:40:58  steveg
 *	update hy_stat to reflect new microcode
 *	
 *	Revision 3.12  84/05/30  19:06:57  steveg
 *	move driver state number definition here from if_hy.c
 *	
 *	Revision 3.11  84/05/30  18:56:15  steveg
 *	add definition of HYE_MAX and HYE_SIZE
 *	
 *	Revision 3.10  84/05/30  17:14:04  steveg
 *	add hyl_filter
 *	
 *	Revision 3.9  84/05/30  13:45:24  steveg
 *	rework logging
 *	
 *	Revision 3.8  84/05/04  05:18:59  steveg
 *	hyr_key now a u_long
 *	
 *	Revision 3.7  84/05/01  22:45:20  steveg
 *	add H_RLOOPBK for A710 remote end loopback command
 *	
 *
 */


/*
 * Structure of a HYPERchannel adapter header
 */
struct	hy_hdr {
	short	hyh_ctl;		/* control */
	short	hyh_access;		/* access code */
	union {
		short	hyh_addr;
		char	hyh_baddr[2];
	} hyh_uto, hyh_ufrom;		/* to/from address */
	short	hyh_param;		/* parameter word */
	short	hyh_type;		/* record type */
};


#define hyh_to		hyh_uto.hyh_addr
#define hyh_to_port	hyh_uto.hyh_baddr[1]
#define hyh_to_adapter	hyh_uto.hyh_baddr[0]

#define hyh_from	hyh_ufrom.hyh_addr
#define hyh_from_port	hyh_ufrom.hyh_baddr[1]
#define hyh_from_adapter hyh_ufrom.hyh_baddr[0]

/*
 * Structure of a HYPERchannel message header (from software)
 */
struct	hym_hdr {
	struct {
		short	hymd_mplen;	/* message proper len, if associated data */
	} hym_d;
	struct	hy_hdr hym_h;	/* hardware header, MUST BE LAST */
};

#define hym_mplen	hym_d.hymd_mplen

#define hym_ctl		hym_h.hyh_ctl
#define hym_access	hym_h.hyh_access
#define hym_param	hym_h.hyh_param
#define hym_type	hym_h.hyh_type

#define hym_to		hym_h.hyh_to
#define hym_to_port	hym_h.hyh_to_port
#define hym_to_adapter	hym_h.hyh_to_adapter

#define hym_from	hym_h.hyh_from
#define hym_from_port	hym_h.hyh_from_port
#define hym_from_adapter hym_h.hyh_from_adapter

#define HYM_SWLEN (sizeof(struct hym_hdr) - sizeof(struct hy_hdr))

/*
 * HYPERchannel header word control bits
 */
#define H_XTRUNKS	0x00F0	/* transmit trunks */
#define H_RTRUNKS	0x000F	/* remote trunks to transmit on for loopback */
#define H_ASSOC		0x0100	/* has associated data */
#define H_LOOPBK	0x00FF	/* loopback command */
#define H_RLOOPBK	0x008F	/* A710 remote loopback command */

/*
 * Hyperchannel record types
 */
#define HYLINK_IP	0	/* Internet Protocol Packet */

/*
 * Routing database
 */
#define HYRSIZE  37	/* max number of adapters in routing tables */

struct hy_route {
	time_t hyr_lasttime;		/* last update time */
	u_char hyr_gateway[256];
	struct hyr_hash {
		u_long	hyr_key;	/* desired address */
		u_short hyr_flags;	/* status flags - see below */
		u_short hyr_size;	/* number of entries */
		union {
			/*
			 * direct entry (can get there directly)
			 */
			struct {
				u_short hyru_dst;	/* adapter number & port */
				u_short hyru_ctl;	/* trunks to try */
				u_short hyru_access;	/* access code (mostly unused) */
			} hyr_d;
#define hyr_dst		hyr_u.hyr_d.hyru_dst
#define hyr_ctl		hyr_u.hyr_d.hyru_ctl
#define hyr_access	hyr_u.hyr_d.hyru_access
			/*
			 * indirect entry (one or more hops required)
			 */
			struct {
				u_char hyru_pgate;	/* 1st gateway slot */
				u_char hyru_egate;	/* # gateways */
				u_char hyru_nextgate;	/* gateway to use next */
			} hyr_i;
#define hyr_pgate	hyr_u.hyr_i.hyru_pgate
#define hyr_egate	hyr_u.hyr_i.hyru_egate
#define hyr_nextgate	hyr_u.hyr_i.hyru_nextgate
		} hyr_u;
	} hyr_hash[HYRSIZE];
};

/*
 * routing table set/get structure
 *
 * used to just pass the entire routing table through, but 4.2 ioctls
 * limit the data part of an ioctl to 128 bytes or so and use the
 * interface name to get things sent the right place.
 * see ../net/if.h for additional details.
 */
struct hyrsetget {
	char	hyrsg_name[IFNAMSIZ];	/* if name, e.g. "hy0" */
	struct hy_route *hyrsg_ptr;	/* pointer to routing table */
	unsigned	hyrsg_len;	/* size of routing table provided */
};

#define HYR_INUSE	0x01	/* entry in use */
#define HYR_DIR		0x02	/* direct entry */
#define HYR_GATE	0x04	/* gateway entry */
#define HYR_LOOP	0x08	/* hardware loopback entry */
#define HYR_RLOOP	0x10	/* remote adapter hardware loopback entry */

#define HYRHASH(x) (((x) ^ ((x) >> 16)) % HYRSIZE)

#define HYSETROUTE	_IOW('i', 0x80, struct hyrsetget)
#define HYGETROUTE	_IOW('i', 0x81, struct hyrsetget)

struct	hylsetget {
	char	hylsg_name[IFNAMSIZ];	/* if name, e.g. "hy0" */
	int	hylsg_cmd;		/* logging command */
	caddr_t	hylsg_ptr;		/* pointer to table */
	u_long	hylsg_len;		/* size of table provided */
};	

#define HYSETLOG	_IOW('i', 0x82, struct hylsetget)
#define HYGETLOG	_IOW('i', 0x83, struct hylsetget)
#define HYGETELOG	_IOW('i', 0x84, struct hylsetget)

/*
 * Structure of Statistics Record (counters)
 */
struct	hy_stat {
	u_char	hyc_df0[3];		/* # data frames trunk 0 */
	u_char	hyc_df1[3];		/* # data frames trunk 1 */
	u_char	hyc_df2[3];		/* # data frames trunk 2 */
	u_char	hyc_df3[3];		/* # data frames trunk 3 */
	u_char	hyc_cancel[2];		/* # cancel operations */
	u_char	hyc_abort[2];		/* # aborts */
	u_char	hyc_ret0[3];		/* # retransmissions trunk 0 */
	u_char	hyc_ret1[3];		/* # retransmissions trunk 1 */
	u_char	hyc_ret2[3];		/* # retransmissions trunk 2 */
	u_char	hyc_ret3[3];		/* # retransmissions trunk 3 */
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

#define HYL_SIZE 16*1024
struct hy_log {
	struct	hy_log *hyl_self;
	u_char	hyl_enable;		/* logging enabled? */
	u_char	hyl_onerr;		/* state to enter on error */
	u_short	hyl_wait;		/* number of bytes till next wakeup */
	u_short	hyl_count;		/* number of samples till stop */
	u_short hyl_icount;		/* initial value of hyl_count */
	u_long	hyl_filter;		/* log items with specific bits set */
	u_char	*hyl_eptr;		/* &hy_log.hyl_buf[HYL_SIZE] */
	u_char	*hyl_ptr;		/* pointer into hyl_buf */
	u_char	hyl_buf[HYL_SIZE];	/* log buffer space */
};

#define HYL_NOP		0
#define HYL_UP		1	/* markup */
#define HYL_STATUS	2	/* status results (struct hy_status) */
#define HYL_STATISTICS	3	/* statistics (struct hy_stat) */
#define HYL_XMIT	4	/* packed being send (struct hym_hdr) */
#define HYL_RECV	5	/* recieved pkt (short len; struct hym_hdr) */
#define HYL_CMD		6	/* cmd issued (uchar cmd, state; short count) */
#define HYL_INT		7	/* interrupt (short csr, wcr) */
#define	HYL_CANCEL	8	/* cancel transmit attempt */
#define	HYL_RESET	9	/* hyinit or unibus reset */
#define	HYL_IOCTL	10	/* hyioctl */

#define HYL_DISABLED	0	/* logging disabled */
#define HYL_CONTINUOUS	1	/* continuous logging */
#define HYL_CATCHN	2	/* hyl_count transactions being captured */

/*
 * error code histograms
 */
#define	HYE_MAX		0x18		/* maximum adapter error code */
#define	HYE_BINS	4		/* number of command bins */
#define	HYE_SIZE  (HYE_MAX+1)*HYE_BINS	/* size of histogram buffer */

/*
 * Requests for service (in order by descending priority).
 */
#define RQ_ENDOP	001	/* end the last adapter function */
#define RQ_REISSUE	002	/* reissue previous cmd after status */
#define RQ_STATUS	004	/* get the status of the adapter */
#define RQ_STATISTICS	010	/* get the statistics of the adapter */
#define RQ_MARKDOWN	020	/* mark this adapter port down */
#define RQ_MARKUP	040	/* mark this interface up */

#define RQ_XASSOC	0100	/* associated data to transmit */

/* 
 * Driver states.
 */
#define	STARTUP		0	/* initial state (before fully there) */
#define	IDLE		1	/* idle state */
#define	STATSENT	2	/* status cmd sent to adapter */
#define	ENDOPSENT	3	/* end operation cmd sent */
#define	RECVSENT	4	/* input message cmd sent */
#define	RECVDATASENT	5	/* input data cmd sent */
#define	XMITSENT	6	/* transmit message cmd sent */
#define	XMITDATASENT	7	/* transmit data cmd sent */
#define	WAITING		8	/* waiting for messages */
#define	CLEARSENT	9	/* clear wait for message cmd sent */
#define MARKPORT	10	/* mark this host's adapter port down issued */
#define RSTATSENT	11	/* read statistics cmd sent to adapter */

#ifdef HYLOG
char *hy_state_names[] = {
	"Startup",
	"Idle",
	"Status Sent",
	"End op Sent",
	"Recieve Message Proper Sent",
	"Recieve Data Sent",
	"Transmit Message Proper Sent",
	"Transmit Data Sent",
	"Wait for Message Sent",
	"Clear Wait for Message Sent",
	"Mark Port Down Sent",
	"Read Statistics Sent"
};
#endif

