/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
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
 *	@(#)mscp.h	7.5 (Berkeley) 6/28/90
 */

/*
 * Definitions for the Mass Storage Control Protocol
 * I WISH I KNEW WHAT MORE OF THESE WERE.  IT SURE WOULD BE NICE
 * IF DEC SOLD DOCUMENTATION FOR THEIR OWN CONTROLLERS.
 */

/*
 * Control message opcodes
 */
#define	M_OP_ABORT	0x01	/* Abort command */
#define	M_OP_GETCMDST	0x02	/* Get command status command */
#define	M_OP_GETUNITST	0x03	/* Get unit status command */
#define	M_OP_SETCTLRC	0x04	/* Set controller characteristics command */
#define	M_OP_SEREX	0x07	/* Serious exception end message */
#define	M_OP_AVAILABLE	0x08	/* Available command */
#define	M_OP_ONLINE	0x09	/* Online command */
#define	M_OP_SETUNITC	0x0a	/* Set unit characteristics command */
#define	M_OP_DTACCPATH	0x0b	/* Determine access paths command */
#define	M_OP_ACCESS	0x10	/* Access command */
#define	M_OP_COMPCD	0x11	/* Compare controller data command */
#define	M_OP_ERASE	0x12	/* Erase command */
#define	M_OP_FLUSH	0x13	/* Flush command */
#define	M_OP_REPLACE	0x14	/* Replace command */
#define	M_OP_COMPHD	0x20	/* Compare host data command */
#define	M_OP_READ	0x21	/* Read command */
#define	M_OP_WRITE	0x22	/* Write command */
#define	M_OP_AVAILATTN	0x40	/* Available attention message */
#define	M_OP_DUPUNIT	0x41	/* Duplicate unit number attention message */
#define	M_OP_ACCPATH	0x42	/* Access path attention message */
#define	M_OP_END	0x80	/* End message flag */


/*
 * Generic command modifiers
 */
#define	M_MD_EXPRS	0x8000	/* Express request */
#define	M_MD_COMP	0x4000	/* Compare */
#define	M_MD_CLSEX	0x2000	/* Clear serious exception */
#define	M_MD_ERROR	0x1000	/* Force error */
#define	M_MD_SCCHH	0x0800	/* Suppress caching (high speed) */
#define	M_MD_SCCHL	0x0400	/* Suppress caching (low speed) */
#define	M_MD_SECOR	0x0200	/* Suppress error correction */
#define	M_MD_SEREC	0x0100	/* Suppress error recovery */
#define	M_MD_SSHDW	0x0080	/* Suppress shadowing */
#define	M_MD_WBKNV	0x0040	/* Write back (non-volatile) */
#define	M_MD_WBKVL	0x0020	/* Write back (volatile) */
#define	M_MD_WRSEQ	0x0010	/* Write shadow set one unit at a time */

/*
 * AVAILABLE command modifiers
 */
#define	M_AVM_ALLCD	0x0002	/* All class drivers */
#define	M_AVM_SPINDOWN	0x0001	/* Spin down */

/*
 * FLUSH command modifiers
 */
#define	M_FLM_FLUSHENU	0x0001	/* Flush entire unit */
#define	M_FLM_VOLATILE	0x0002	/* Volatile only */

/*
 * GET UNIT STATUS command modifiers
 */
#define	M_GUM_NEXTUNIT	0x0001	/* Next unit */

/*
 * ONLINE command modifiers
 */
#define	M_OLM_RIP	0x0001	/* Allow self destruction */
#define	M_OLM_IGNMF	0x0002	/* Ignore media format error */

/*
 * ONLINE and SET UNIT CHARACTERISTICS command modifiers
 */
#define	M_OSM_ALTERHI	0x0020	/* Alter host identifier */
#define	M_OSM_SHADOWSP	0x0010	/* Shadow unit specified */
#define	M_OSM_CLEARWBL	0x0008	/* Clear write-back data lost */
#define	M_OSM_SETWRPROT	0x0004	/* Set write protect */

/*
 * REPLACE command modifiers
 */
#define	M_RPM_PRIMARY	0x0001	/* Primary replacement block */

/*
 * End message flags
 */
#define	M_EF_BBLKR	0x80	/* Bad block reported */
#define	M_EF_BBLKU	0x40	/* Bad block unreported */
#define	M_EF_ERLOG	0x20	/* Error log generated */
#define	M_EF_SEREX	0x10	/* Serious exception */

/*
 * Controller flags
 */
#define	M_CF_ATTN	0x80	/* Enable attention messages */
#define	M_CF_MISC	0x40	/* Enable miscellaneous error log messages */
#define	M_CF_OTHER	0x20	/* Enable other host's error log messages */
#define	M_CF_THIS	0x10	/* Enable this host's error log messages */
#define	M_CF_MLTHS	0x04	/* Multi-host */
#define	M_CF_SHADW	0x02	/* Shadowing */
#define	M_CF_576	0x01	/* 576 byte sectors */

/*
 * Unit flags
 */
#define	M_UF_REPLC	0x8000	/* Controller initiated bad block replacement */
#define	M_UF_INACT	0x4000	/* Inactive shadow set unit */
#define	M_UF_WRTPH	0x2000	/* Write protect (hardware) */
#define	M_UF_WRTPS	0x1000	/* Write protect (software or volume) */
#define	M_UF_SCCHH	0x8000	/* Suppress caching (high speed) */
#define	M_UF_SCCHL	0x4000	/* Suppress caching (low speed) */
#define	M_UF_RMVBL	0x0080	/* Removable media */
#define	M_UF_WBKNV	0x0040	/* Write back (non-volatile) */
#define	M_UF_576	0x0004	/* 576 byte sectors */
#define	M_UF_CMPWR	0x0002	/* Compare writes */
#define	M_UF_CMPRD	0x0001	/* Compare reads */

/*
 * Error Log message format codes
 */
#define	M_FM_CTLRERR	0x00	/* Controller error */
#define	M_FM_BUSADDR	0x01	/* Host memory access error */
#define	M_FM_DISKTRN	0x02	/* Disk transfer error */
#define	M_FM_SDI	0x03	/* SDI error */
#define	M_FM_SMLDSK	0x04	/* Small disk error */

/*
 * Error Log message flags
 */
#define	M_LF_SUCC	0x80	/* Operation successful */
#define	M_LF_CONT	0x40	/* Operation continuing */
#define	M_LF_SQNRS	0x01	/* Sequence number reset */

/*
 * Status codes
 */
#define	M_ST_MASK	0x1f	/* Status code mask */
#define	M_ST_SUCCESS	0x00	/* Success */
#define	M_ST_INVALCMD	0x01	/* Invalid command */
#define	M_ST_ABORTED	0x02	/* Command aborted */
#define	M_ST_OFFLINE	0x03	/* Unit offline */
#define	M_ST_AVAILABLE	0x04	/* Unit available */
#define	M_ST_MFMTERR	0x05	/* Media format error */
#define	M_ST_WRPROT	0x06	/* Write protected */
#define	M_ST_COMPERR	0x07	/* Compare error */
#define	M_ST_DATAERR	0x08	/* Data error */
#define	M_ST_HOSTBUFERR	0x09	/* Host buffer access error */
#define	M_ST_CTLRERR	0x0a	/* Controller error */
#define	M_ST_DRIVEERR	0x0b	/* Drive error */
#define	M_ST_DIAG	0x1f	/* Message from an internal diagnostic */

/*
 * Subcodes of M_ST_OFFLINE
 */
#define	M_OFFLINE_UNKNOWN	(0 << 5) /* unknown or on other ctlr */
#define	M_OFFLINE_UNMOUNTED	(1 << 5) /* unmounted or RUN/STOP at STOP */
#define	M_OFFLINE_INOPERATIVE	(2 << 5) /* inoperative? */
#define	M_OFFLINE_DUPLICATE	(4 << 5) /* duplicate unit number */
#define	M_OFFLINE_INDIAGNOSTIC	(8 << 5) /* disabled by FS or diagnostic */

/*
 * An MSCP packet begins with a header giving the length of
 * the entire packet (including the header itself)(?), two bytes
 * of device specific data, and the a whole bunch of variants
 * depending on message type.
 *
 * N.B.:  In most cases we distinguish between a `command' and
 * an `end' variant as well.  The command variant is that which
 * is given to the controller; the `end' variant is its response.
 */

/*
 * Generic sequential message variant (command and response).
 */
struct mscpv_seq {
	long	seq_bytecount;		/* byte count */
#define	seq_rbn		seq_bytecount	/* aka RBN (replace) */
#define	seq_outref	seq_bytecount	/* aka outref (abort/get cmd status) */
	long	seq_buffer;		/* buffer descriptor */
	long	seq_mapbase;		/* page map (first PTE) phys address */
	long	seq_xxx1;	/* ? */	/* unused */
	long	seq_lbn;		/* logical block number */
	long	seq_xxx2;	/* ? */	/* unused */
	long	*seq_addr;		/* pointer to cmd descriptor */
	long	seq_software[4];	/* reserved to software; unused */
};

/*
 * Set Controller Characteristics command variant
 */
struct mscpv_sccc {
	u_short	sccc_version;		/* MSCP version number */
	u_short	sccc_ctlrflags;		/* controller flags */
	u_short	sccc_hosttimo;		/* host timeout */
	u_short	sccc_usefrac;		/* use fraction */
	long	sccc_time;		/* time and date */
	long	sccc_xxx1;	/* ? */
	long	sccc_errlgfl;	/* ? */
	short	sccc_xxx2;	/* ? */
	short	sccc_copyspd;	/* ? */
};

/*
 * Set Controller Characteristics end variant
 */
struct mscpv_scce {
	u_short	scce_version;		/* MSCP version number */
	u_short	scce_ctlrflags;		/* controller flags */
	u_short	scce_ctlrtimo;		/* controller timeout */
	u_short	scce_ctlrcmdl;		/* ??? */
	quad	scce_ctlrid;		/* controller ID */
	long	scce_xxx[3];	/* ? */
	long	scce_volser;		/* volume serial number */
};

/*
 * On Line command variant
 */
struct mscpv_onlc {
	long	onlc_xxx1[4];	/* ? */
	long	onlc_errlgfl;		/* error log flag? */
	short	onlc_xxx2;	/* ? */
	short	onlc_copyspd;		/* copy speed? */
};

/*
 * On Line end variant
 */
struct mscpv_onle {
	long	onle_xxx1[3];	/* ? */
/*???*/	short	onle_xxx2;	/* ? */
	u_char	onle_drivetype;		/* drive type index (same in guse) */
	char	onle_xxx3;	/* ? */
	long	onle_mediaid;		/* media type id (same in guse) */
	long	onle_xxx4;	/* ? */
	long	onle_unitsize;		/* unit size in sectors */
	long	onle_volser;		/* volume serial number */
};

/*
 * Get Unit Status end variant (and Avail Attn?)
 */
struct mscpv_guse {
	u_short	guse_multunit;		/* multi-unit code */
	u_short	guse_unitflags;		/* unit flags */
	long	guse_hostid;		/* host id */
	long	guse_unitid0;	/*???*/
	short	guse_unitid1;	/*???*/
	u_char	guse_drivetype;		/* drive type index */
	u_char	guse_unitid2;	/*???*/
	long	guse_mediaid;		/* media type id (encoded) */
	short	guse_shadowunit;	/* shadow unit */
	short	guse_shadowstat;	/* shadow status */
	u_short	guse_nspt;		/* sectors per track */
	u_short	guse_group;		/* track group size */
	u_short	guse_ngpc;		/* groups per cylinder */
	u_short	guse_xxx;		/* reserved */
	u_short	guse_rctsize;		/* RCT size (sectors) */
	u_char	guse_nrpt;		/* RBNs per track */
	u_char	guse_nrct;		/* number of RCTs */
};

/*
 * Macros to break up and build media IDs.  An ID encodes the port
 * type in the top 10 bits, and the drive type in the remaining 22.
 * The 10 bits, and 15 of the 22, are in groups of 5, with the value
 * 0 representing space and values 1..26 representing A..Z.  The low
 * 7 bits represent a number in 0..127.  Hence an RA81 on a UDA50
 * is <D><U><R><A>< >81, or 0x25641051.  This encoding scheme is known
 * in part in uda.c.
 *
 * The casts below are just to make pcc generate better code.
 */
#define	MSCP_MEDIA_PORT(id)	(((long)(id) >> 22) & 0x3ff)	/* port */
#define	MSCP_MEDIA_DRIVE(id)	((long)(id) & 0x003fffff)	/* drive */
#define	MSCP_MID_ECH(n, id)	(((long)(id) >> ((n) * 5 + 7)) & 0x1f)
#define	MSCP_MID_CHAR(n, id) \
	(MSCP_MID_ECH(n, id) ? MSCP_MID_ECH(n, id) + '@' : ' ')
#define	MSCP_MID_NUM(id)	((id) & 0x7f)
/* for, e.g., RA81 */
#define	MSCP_MKDRIVE2(a, b, n) \
	(((a) - '@') << 17 | ((b) - '@') << 12 | (n))
/* for, e.g., RRD50 */
#define	MSCP_MKDRIVE3(a, b, c, n) \
	(((a) - '@') << 17 | ((b) - '@') << 12 | ((c) - '@') << 7 | (n))

/*
 * Error datagram variant.
 */
struct mscpv_erd {
	quad	erd_ctlrid;		/* controller ID */
	u_char	erd_ctlrsoftware;	/* controller software version */
	u_char	erd_ctlrhardware;	/* controller hardware version */
	u_short	erd_multiunit;		/* multi-unit code (?) */
	union {
		u_long	un_busaddr;	/* bus address, if mem access err */
		quad	un_unitid;	/* unit id, otherwise */
	} erd_un1;
#define	erd_busaddr	erd_un1.un_busaddr
#define	erd_unitid	erd_un1.un_unitid
	u_char	erd_unitsoftware;	/* unit software version */
	u_char	erd_unithardware;	/* unit hardware version */
	union {
		u_char	un_b[2];	/* level, retry (if disk xfer err) */
		u_short	un_s;		/* cylinder (if small disk error) */
	} erd_un2;
#define	erd_level	erd_un2.un_b[0]
#define	erd_retry	erd_un2.un_b[1]
#define	erd_sdecyl	erd_un2.un_s
	long	erd_volser;		/* volume serial number */
	u_long	erd_hdr;		/* `header' (block number) */
	u_char	erd_sdistat[12];	/* SDI status information (?) */
};

/*
 * I am making brash assumptions about the first four bytes of all
 * MSCP packets.  These appear to be true for both UDA50s and TMSCP
 * devices (TU81, TA81, TK50).  DEC claim that these four bytes are
 * not part of MSCP itself, yet at least the length is necessary
 * for, e.g., error checking.
 */
struct mscp {
	u_short	mscp_msglen;		/* length in bytes */
	u_char	mscp_msgtc;		/* type (high 4 bits) and credits */
	u_char	mscp_vcid;		/* virtual circuit ID */
	long	mscp_cmdref;		/* command reference number */
	u_short	mscp_unit;		/* unit number */
	u_short	mscp_seqnum;		/* sequence number */
	u_char	mscp_opcode;		/* opcode */
#define	mscp_format	mscp_opcode	/* aka format (datagrams) */
	u_char	mscp_flags;		/* flags */
	u_short	mscp_modifier;		/* modifier (commands) */
#define	mscp_status	mscp_modifier	/* aka status (ends) */
#define	mscp_event	mscp_modifier	/* aka event (datagrams) */
	union {
		struct	mscpv_seq un_seq;	/* generic sequential msg */
		struct	mscpv_sccc un_sccc;	/* SCC command */
		struct	mscpv_scce un_scce;	/* SCC end */
		struct	mscpv_onlc un_onlc;	/* on line command */
		struct	mscpv_onle un_onle;	/* on line end */
		struct	mscpv_guse un_guse;	/* get unit status */
		struct	mscpv_erd un_erd;	/* error datagram */
	} mscp_un;
/*???*/	long	mscp_xxx;		/* pad to 64 bytes */
};

/*
 * Define message length according to the DEC specifications by dropping
 * the four byte header.
 */
#define	MSCP_MSGLEN	(sizeof (struct mscp) - 4)

/*
 * Shorthand
 */

/*
 * Generic packet
 */
#define	mscp_seq	mscp_un.un_seq

/*
 * Set Controller Characteristics packet
 */
#define	mscp_sccc	mscp_un.un_sccc

/*
 * Set Controller Characteristics end packet
 */
#define	mscp_scce	mscp_un.un_scce

/*
 * Online / Set Unit Characteristics command packet
 */
#define	mscp_onlc	mscp_un.un_onlc

/*
 * Online end packet
 */
#define	mscp_onle	mscp_un.un_onle

/*
 * Get Unit Status end packet
 */
#define	mscp_guse	mscp_un.un_guse

/*
 * MSCP Error Log packet
 */
#define	mscp_erd	mscp_un.un_erd

/*
 * MSCP seq_addr field actually belongs to overall packet.
 */
#define	mscp_addr	mscp_seq.seq_addr

/*
 * Macros to break up mscp_msgtc, and types.
 */
#define	MSCP_MSGTYPE(m)	((m) & 0xf0)
#define	MSCP_CREDITS(m)	((m) & 0x0f)

#define	MSCPT_SEQ		0x00	/* sequential message */
#define	MSCPT_DATAGRAM		0x10	/* error datagram */
#define	MSCPT_CREDITS		0x20	/* credit notification */
#define	MSCPT_MAINTENANCE	0xf0	/* who knows */


/*
 * Here begin more perhaps brash assumptions about MSCP devices...
 */

/*
 * MSCP controllers have `command rings' and `response rings'.  A
 * command ring is a pool of MSCP packets that the host uses to give
 * commands to the controller; a response ring is a pool of MSCP
 * packets that the controller uses to give back responses.  Entries
 * in the command and response rings are `owned' by either the host
 * or the controller; only the owner is allowed to alter any of the
 * fields in the MSCP packet.  Thus, free command packets are owned
 * by the host, and free response packets by the controller.  When
 * the host gives a packet to the controller, it tells the controller
 * by touching a device register; when the controller gives a response
 * to the host, it generates an interrupt if enabled, and sets
 * a device register as well.
 *
 * The pool is `described' by a set of pointers to the packets, along
 * with the two flags below.
 */
#define	MSCP_OWN	0x80000000	/* controller owns this packet */
#define	MSCP_INT	0x40000000	/* controller should interrupt */
