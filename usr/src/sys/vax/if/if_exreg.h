/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Excelan Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)if_exreg.h	7.3 (Berkeley) 6/28/90
 */

struct	exdevice {
	char	xd_porta;	/* write on porta resets EXOS */
	char	xd_pad_a;
	char	xd_portb;	/* write on portb interrupts EXOS */
				/* read on portb returns status bits */
	char	xd_pad_b;
};

/* EXOS I/O PORT A write definitions */
#define	EX_RESET	0	/* value doesn't really matter... */

/* EXOS I/O PORT B write definitions */
#define	EX_NTRUPT	0

/* EXOS I/O PORT B read definitions */
#define	EX_TESTOK	1	/* set when self-diagnostics passed */
#define	EX_UNREADY	(1<<3)	/* set until EXOS ready to read from B */

/* message buffer status field definitions */
#define	MH_OWNER	1	/* mask for status bit for owner */
#define	MH_HOST		0	/* if 0, the host owns the buffer */
#define	MH_EXOS		1	/* if 1, the EXOS owns the buffer */

/* EXOS Link Level request codes */
#define	LLTRANSMIT	0xC	/* send a packet */
#define	LLRTRANSMIT	0xE	/* send a packet, and self-receive */
#define	LLRECEIVE	0xD	/* receive a packet */
#define	LLNET_MODE	0x8	/* read/write mode control objects */
#define	LLNET_ADDRS	0x9	/* read/write receive address slots */
#define	LLNET_RECV	0xA	/* read/alter receive slot enable bit */
#define	LLNET_STSTCS	0xB	/* read/reset network statistics objects */

/* Link Level return codes common to all requests */
#define	LL_OK		0	/* successful completion */
#define	LLX_MODE	0xA1	/* EXOS not in link level mode (impossible) */

/* LLTRANSMIT unique return codes */
#define	LLXM_1RTRY	0x1	/* successful xmission, 1 retry */
#define	LLXM_RTRYS	0x2	/* successful xmission, more than 1 retry */
#define	LLXM_NSQE	0x8	/* successful xmission, no SQE TEST signal */
#define	LLXM_CLSN	0x10	/* xmission failed, excess retries */
#define	LLXM_NCS	0x20	/* xmission failed, no carrier sense */
#define	LLXM_LNGTH	0x40	/* xmission failed, bad packet length */
#define	XMIT_BITS	"\7\7LENGTH\6CARRIER\5XCLSNS\4SQETST"
#define	LLXM_ERROR	(LLXM_NSQE|LLXM_CLSN|LLXM_NCS|LLXM_LNGTH)

/* LLRECEIVE unique return codes */
#define	LLRC_TRUNC	0x4	/* pkt received, but truncated to fit buffer */
#define	LLRC_ALIGN	0x10	/* pkt received, but with alignment error */
#define	LLRC_CRC	0x20	/* pkt received, but with CRC error */
#define	LLRC_BUFLEN	0x40	/* no pkt received, buffer less than 64 bytes */
				/* this should never happen here */
#define	RECV_BITS	"\7\7BUFLEN\6CRC\5ALIGN\3TRUNC"

/* LLNET_ADDRS unique return codes */
#define	LLNA_BADSLOT	0xD1	/* slot doesn't exist or can't be accessed */
#define	LLNA_BADADDR	0xD3	/* invalid address for designated slot */

/* LLNET_RECV unique return codes */
#define	LLNR_BADSLOT	0xD1	/* slot doesn't exist or can't be accessed */
#define	LLNR_BADADDR	0xD2	/* designated slot was empty */

/* address slot object indices */
#define	NULLSLOT	0	/* the null slot */
#define	MINMCSLOT	1	/* minimum multicast slot index */
#define	MAXMCSLOT	8	/* default maximum multicast slot index */
#define	PHYSSLOT	253	/* physical slot index */
#define	UNVRSSLOT	254	/* universal slot index */
#define	BROADSLOT	255	/* broadcast slot index */

/* request mask bit definitions */
#define	WRITE_OBJ	1	/* write the object */
#define	READ_OBJ	2	/* read the object */
#define	ENABLE_RCV	4	/* enable reception on designated slot */

/* NET_MODE options mask bit definitions */
#define	OPT_ALIGN	0x10	/* receive packets with alignment errors */
#define	OPT_CRC		0x20	/* receive packets with CRC errors */
#define	OPT_DSABLE	0x80	/* disconnect controller hardware */

/* NET_MODE mode field value definitions */
#define	MODE_OFF	0	/* stop transmission and reception */
#define	MODE_PERF	1	/* perfect multicast address filtering */
#define	MODE_HW		2	/* hardware-only multicast address filtering */
#define	MODE_PROM	3	/* promiscuous reception */

#define	NFRAGMENTS 1	/* number fragments that the EXOS will scatter/gather */
#define	EXMAXRBUF 1520	/* per EXOS 101 manual 5.3.7 (maybe 1518 would do) */

/*
 * N.B.  Structures below are carefully constructed so that
 * they correspond to the message formats that NX firmware
 * defines.  None of them should contain any compiler-instigated
 * padding.  Be especially careful about VAX C longword alignment!
 */

struct	stat_array {
	u_long	sa_fsent;	/* frames sent without errors */
	u_long	sa_xsclsn;	/* frames aborted excess collisions */
	u_long	sa_nsqe;	/* frames subject to heartbeat failure */
	u_long	sa_undef;	/* undefined (TDR on EXOS 101) */
	u_long	sa_frcvd;	/* frames received no errors */
	u_long	sa_align;	/* frames received alignment error */
	u_long	sa_crc;		/* frames received crc error */
	u_long	sa_flost;	/* frames lost */
};

struct	buf_blk {		/* packet/buffer block descriptor */
	u_short	bb_len;			/* length of block, in bytes */
	u_short	bb_addr[2];		/* address of block */
	/*
	 * Array above is really a single u_long field.
	 * We kludge its definition to defeat word-alignment.
	 * Access would look like:
	 *     longaddr = *(u_long *)bp->.mb_er.er_blks[0].bb_addr;
	 */
};

struct	net_mode {		/* read/write mode control objects */
/*12*/	u_char	nm_rqst;	/* request code */
/*13*/	u_char	nm_rply;	/* reply code */
/*14*/	u_char	nm_mask;		/* bit-wise switches for read, write */
/*15*/	u_char	nm_optn;		/* acceptable packet reception errors */
/*16*/	u_char	nm_mode;		/* EXOS filtering mode */
/*17*/
};

struct	net_addrs {		/* read/write receive address slots */
/*12*/	u_char	na_rqst;	/* request code */
/*13*/	u_char	na_rply;	/* reply code */
/*14*/	u_char	na_mask;		/* bit-wise switches for read, write */
/*15*/	u_char	na_slot;		/* index of address slot */
/*16*/	u_char	na_addrs[6];		/* address read and/or written */
/*22*/
};

struct	net_recv {		/* read/alter receive slot enable bit */
/*12*/	u_char	nr_rqst;	/* request code */
/*13*/	u_char	nr_rply;	/* reply code */
/*14*/	u_char	nr_mask;		/* bit-wise switches for read, write */
/*15*/	u_char	nr_slot;		/* index of address slot */
/*16*/
};

struct	net_ststcs {		/* read/reset network statistics objects */
/*12*/	u_char	ns_rqst;	/* request code */
/*13*/	u_char	ns_rply;	/* reply code */
/*14*/	u_char	ns_mask;		/* bit-wise switches for read, write */
/*15*/	u_char	ns_rsrv;		/* reserved for EXOS */
/*16*/	u_short	ns_nobj;		/* number of objects to work on */
/*18*/	u_short	ns_xobj;		/* index of first object to work on */
/*20*/	u_long	ns_bufp;		/* pointer to statistics buffer */
/*24*/
};

struct	enet_xmit {		/* send a packet on the Ethernet */
/*12*/	u_char	et_rqst;	/* request code */
/*13*/	u_char	et_rply;	/* reply code */
/*14*/	u_char	et_slot;		/* address slot matching dest address */
/*15*/	u_char	et_nblock;		/* number of blocks composing packet */
/*16*/	struct	buf_blk et_blks[NFRAGMENTS];	/* array of block descriptors */
/*22-64*/
};

struct	enet_recv {		/* receive a packet on the Ethernet */
/*12*/	u_char	er_rqst;	/* request code */
/*13*/	u_char	er_rply;	/* reply code */
/*14*/	u_char	er_slot;		/* address slot matching dest address */
/*15*/	u_char	er_nblock;		/* number of blocks composing buffer */
/*16*/	struct	buf_blk er_blks[NFRAGMENTS];	/* array of block descriptors */
/*22-64*/
};

/* we send requests and receive replys with the EXOS using this structure */
struct	ex_msg {
/*00*/	u_short	mb_link;	/* address of next message buffer */
/*02*/	u_char	mb_rsrv;	/* reserved for use by EXOS */
/*03*/	u_char	mb_status;	/* used bit-wise for message protocol */
/*04*/	u_short	mb_length;	/* length, in bytes, of the rest */
/*06*/	short	mb_1rsrv;	/* reserved for used by EXOS */
/*08*/	long	mb_mid;		/* available to user */
/*12*/	union	mb_all {
		struct	net_mode	mb_net_mode;
		struct	net_addrs	mb_net_addrs;
		struct	net_recv	mb_net_recv;
		struct	net_ststcs	mb_net_ststcs;
		struct	enet_xmit	mb_enet_xmit;
		struct	enet_recv	mb_enet_recv;
	} mb_all;
/* following field is used only by host, not read by board */
	struct	ex_msg *mb_next;	/* host's pointer to next message */
};
#define	mb_nm	mb_all.mb_net_mode
#define	mb_na	mb_all.mb_net_addrs
#define	mb_nr	mb_all.mb_net_recv
#define	mb_ns	mb_all.mb_net_ststcs
#define	mb_et	mb_all.mb_enet_xmit
#define	mb_er	mb_all.mb_enet_recv
#define	mb_rqst	mb_nm.nm_rqst
#define	mb_rply	mb_nm.nm_rply
#define	MBDATALEN (sizeof(union mb_all)+6)

struct	confmsg {
/*00*/	u_short	cm_1rsrv;	/* reserved, must be 1 */
/*02*/	char	cm_vc[4];	/* returns ASCII version code */
/*06*/	u_char	cm_cc;		/* returns config completion code */
/*07*/	u_char	cm_opmode;	/* specifies operation mode */
/*08*/	u_short	cm_dfo;		/* specifies host data format option */
/*10*/	u_char	cm_dcn1;	/* reserved, must be 1 */
/*11*/	u_char	cm_2rsrv[2];	/* reserved, must be 0 */
/*13*/	u_char	cm_ham;		/* specifies host address mode */
/*14*/	u_char	cm_3rsrv;	/* reserved, must be 0 */
/*15*/	u_char	cm_mapsiz;	/* reserved, must be 0 */
/*16*/	u_char	cm_byteptrn[4];	/* host data format option test pattern */
/*20*/	u_short	cm_wordptrn[2];
/*24*/	u_long	cm_lwordptrn;
/*28*/	u_char	cm_rsrvd[20];
/*48*/	u_long	cm_mba;		/* use 0xFFFFFFFF in link level mode */
/*52*/	u_char	cm_nproc;	/* use 0xFF in link level mode */
/*53*/	u_char	cm_nmbox;	/* use 0xFF in link level mode */
/*54*/	u_char	cm_nmcast;	/* use 0xFF in link level mode */
/*55*/	u_char	cm_nhost;	/* use 1 in link level mode */

	/* the next five parameters define the request message queue */
/*56*/	u_long	cm_h2xba;	/* base address of message queue */
/*60*/	u_short	cm_h2xhdr;	/* address offset of msg Q header */
/*62*/	u_char	cm_h2xtyp;	/* interrupt type */
/*63*/	u_char	cm_h2xval;	/* interrupt value (not used) */
/*64*/	u_short	cm_h2xaddr;	/* interrupt vector */
/*66*/	u_short	cm_h2xpad;	/* pad out unused portion of vector */

	/* the next five parameters define the reply message queue */
/*68*/	u_long	cm_x2hba;	/* base address of message queue */
/*72*/	u_short	cm_x2hhdr;	/* address offset of msg Q header */
/*74*/	u_char	cm_x2htyp;	/* interrupt type */
/*75*/	u_char	cm_x2hval;	/* interrupt value (not used) */
/*76*/	u_short	cm_x2haddr;	/* interrupt vector */
/*78*/	u_short	cm_x2hpad;	/* pad out unused portion of vector */
/*80*/
};

