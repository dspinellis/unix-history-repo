/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
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
 *	@(#)disk.h	8.1 (Berkeley) 6/10/93
 *
 * from: $Header: disk.h,v 1.4 93/04/30 00:04:10 torek Exp $ (LBL)
 */

/*
 * SCSI definitions for Direct Access Devices (disks).
 * This includes WORMs and CD-ROMs (although a few commands, such as
 * format or write, are nonsensical on some).
 *
 * Commands defined in common headers (scsi.h or disktape.h) appear here
 * as comments.
 */

	/* group 0 */
/*	CMD_TEST_UNIT_READY	0x00	   test unit ready */
#define	CMD_REZERO		0x01	/* rezero unit */
/*	CMD_REQUEST_SENSE	0x03	   request sense */
#define	CMD_FORMAT_UNIT		0x04	/* format unit (disk) */
#define	CMD_REASSIGN_BLOCKS	0x07	/* reassign blocks (disk, WORM) */
#define	CMD_READ6		0x08	/* read (6 byte cdb) */
#define	CMD_WRITE6		0x0a	/* write (6 byte cdb) */
#define	CMD_SEEK6		0x0b	/* seek (6 byte cdb) */
/*	CMD_INQUIRY		0x12	   inquiry */
/*	CMD_MODE_SELECT		0x15	   mode select */
#define	CMD_RESERVE		0x16	/* reserve */
#define	CMD_RELEASE		0x17	/* release */
/*	CMD_COPY		0x18	   copy */
/*	CMD_MODE_SENSE		0x1a	   mode sense */
#define	CMD_SSU			0x1b	/* start/stop unit */
/*	CMD_RECEIVE_DIAG	0x1c	   receive diagnostic results */
/*	CMD_SEND_DIAG		0x1d	   send diagnostic */
#define	CMD_PAMR		0x1e	/* prevent/allow medium removal */

	/* group 1 */
#define	CMD_READ_CAPACITY	0x25	/* read capacity */
#define	CMD_READ10		0x28	/* read (10 byte cdb) */
#define	CMD_WRITE10		0x2a	/* write (10 byte cdb) */
#define	CMD_SEEK10		0x2b	/* write (10 byte cdb) */
#define	CMD_WRITE_VERIFY	0x2e	/* write and verify */
#define	CMD_VERIFY		0x2f	/* verify */
#define	CMD_SEARCH_H		0x30	/* search data high */
#define	CMD_SEARCH_E		0x31	/* search data equal */
#define	CMD_SEARCH_L		0x32	/* search data low */
#define	CMD_SET_LIMITS		0x33	/* set limits */
/*	CMD_COMPARE		0x39	   compare */
#define	CMD_COPY_VERIFY		0x3a	/* copy and verify */

/* this one is in van's but not in my 17B documents */
#define	CMD_READ_DEFECT_DATA	0x37	/* read defect data */ /* ??? */

/*
 * Structure of a FORMAT UNIT command (i.e., the cdb):
 *	byte 0: opcode<8>
 *	byte 1: lun<3> format_data<1> complete_list<1> defect_list_format<3>
 *	byte 2: vendor unique
 *	byte 3: interleave (MSB)
 *	byte 4: interleave (LSB)
 *	byte 5: control
 */
struct scsi_cdb_fu {
	u_char	cdb_cmd;		/* SCSI_CMD_FU */
	u_char	cdb_lun_etc;		/* lun+FD+CL+DLF */
	u_char	cdb_vu;			/* vendor unique */
	u_char	cdb_ilvh;		/* interleave (MSB) */
	u_char	cdb_ilvl;		/* interleave (LSB) */
	u_char	cdb_ctrl;		/* control byte */
};

/*
 * If format data are supplied, they give either additional (cl=0) or
 * new (cl=1) defect list in one of the following formats.
 * Formats 1, 2, and 3 are the same as 0; formats 6 and 7 are
 * vendor unique and reserved, respectively.  (The `backwards'
 * in `backwards compatible'...)
 */
#define	SCSI_DLF_BLOCK	0		/* dlf = blocks */
#define	SCSI_DLF_BFI	4		/* dlf = bytes from index */
#define	SCSI_DLF_PS	5		/* dlf = physical sectors */

/*
 * Defect list header, block format (`defect block address').
 *
 * N.B.: this structure is also used for the Reassign Blocks command;
 * there the `defect block address' becomes a `defect logical block address'.
 */
struct scsi_dlf_dba {
	u_short	dlf_xxx;		/* reserved */
	u_char	dlf_lenh,		/* defect list length (MSB) */
		dlf_lenl;		/* defect list length (LSB) */
	struct scsi_dlf_dba_desc {
		u_char	dbah,		/* defect block address (MSB) */
			dbahm,		/* defect block address */
			dbalm,		/* defect block address */
			dbal;		/* defect block address (LSB) */
	} dlf_dba[1];			/* actually longer */
};

/*
 * Defect list header, Bytes From Index format.
 */
struct scsi_dlf_bfi {
	u_short	dlf_xxx;		/* reserved */
	u_char	dlf_lenh,		/* defect list length (MSB) */
		dlf_lenl;		/* defect list length (LSB) */
	struct scsi_dlf_bfi_desc {
		u_char	cylh,		/* cylinder number of defect (MSB) */
			cylm,		/* cylinder number of defect */
			cyll,		/* cylinder number of defect (LSB) */
			head,		/* head number of defect */
			bfih,		/* defect bytes from index (MSB) */
			bfihm,		/* defect bytes from index */
			bfilm,		/* defect bytes from index */
			bfil;		/* defect bytes from index (LSB) */
	} dlf_bfi[1];			/* actually longer */
};

/*
 * Defect list header, Physical Sector format.
 */
struct scsi_dlf_ps {
	u_short	dlf_xxx;		/* reserved */
	u_char	dlf_lenh,		/* defect list length (MSB) */
		dlf_lenl;		/* defect list length (LSB) */
	struct scsi_dlf_ps_desc {
		u_char	cylh,		/* cylinder number of defect (MSB) */
			cylm,		/* cylinder number of defect */
			cyll,		/* cylinder number of defect (LSB) */
			head,		/* head number of defect */
			dsnh,		/* defect sector number (MSB) */
			dsnhm,		/* defect sector number */
			dsnlm,		/* defect sector number */
			dsnl;		/* defect sector number (LSB) */
	} dlf_ps[1];			/* actually longer */
};

/*
 * For MODE SENSE and MODE SELECT: Mode page codes for disks.
 */
/*				0x00	   vendor specific */
#define	SCSI_MS_PC_RWERRREC	0x01	/* r/w error recovery parameters */
/*	SCSI_MS_PC_DR		0x02	   disconnect/reconnect control */
#define	SCSI_MS_PC_FMT		0x03	/* format parameters */
#define	SCSI_MS_PC_RDGEOM	0x04	/* Rigid Disk geometry */
#define	SCSI_MS_PC_FD		0x05	/* flexible disk page */
/*				0x06	   reserved */
#define	SCSI_MS_PC_VERRREC	0x07	/* verify error recovery page */
#define	SCSI_MS_PC_CACHE	0x08	/* cache page */
/*	SCSI_MS_PC_PDEV		0x09	   peripheral device page */
/*	SCSI_MS_PC_CTLMODE	0x0a	   control mode page */
#define	SCSI_MS_PC_MTSUPP	0x0b	/* medium types supported */
#define	SCSI_MS_PC_NOTCH	0x0c	/* notch page */
/*				0x0d..0x1f reserved */
/*				0x20..0x3e vendor specific */
#define	SCSI_MS_PC_CDCCACHECTL	0x38	/* CDC (Wren) cache control page */

/*
 * Structure of a Read/Write Error Recovery mode page.
 * N.B.: CDC Wren V, at least, does not include write retry & time limit.
 */
struct scsi_page_rwerrrec {
	u_char	rw_flags,	/* flags, see below */
		rw_read_retry,	/* read retry count */
		rw_corr_span,	/* correction span */
		rw_hd_off,	/* head offset count */
		rw_ds_off,	/* data strobe offset count */
		rw_xxx0,	/* reserved */
		rw_write_retry,	/* write retry count */
		rw_xxx1,	/* reserved */
		rw_rtlh,	/* recovery time limit (MSB) */
		rw_rtll;	/* recovery time limit (LSB) */
};
/* rw_flags */
#define	SCSI_RWE_AWRE	0x80	/* reallocate defective blocks on write */
#define	SCSI_RWE_ARRE	0x40	/* reallocate defective blocks on read */
#define	SCSI_RWE_TB	0x20	/* transfer unrecoverable block */
#define	SCSI_RWE_RC	0x10	/* recovery may not cause delay: may lie */
#define	SCSI_RWE_EER	0x08	/* use most expedient recovery, not best */
#define	SCSI_RWE_PER	0x04	/* report recovered errors */
#define	SCSI_RWE_DTE	0x02	/* stop after recovered error */
#define	SCSI_RWE_DCR	0x01	/* use ECC for detection only */

/*
 * Structure of a Format Device mode page.
 */
struct scsi_page_fmt {
	u_char	fmt_tpzh,	/* tracks per zone (MSB) */
		fmt_tpzl,	/* tracks per zone (LSB) */
		fmt_aspzh,	/* alternate sectors per zone (MSB) */
		fmt_aspzl,	/* alternate sectors per zone (LSB) */
		fmt_atpzh,	/* alternate tracks per zone (MSB) */
		fmt_atpzl,	/* alternate tracks per zone (LSB) */
		fmt_atpvh,	/* alternate tracks per volume (MSB) */
		fmt_atpvl,	/* alternate tracks per volume (LSB) */
		fmt_spth,	/* sectors per track (MSB) */
		fmt_sptl,	/* sectors per track (LSB) */
		fmt_dbppsh,	/* data bytes per physical sector (MSB) */
		fmt_dbppsl,	/* data bytes per physical sector (LSB) */
		fmt_ilh,	/* interleave (MSB) */
		fmt_ill,	/* interleave (LSB) */
		fmt_tsfh,	/* track skew factor (MSB) */
		fmt_tsfl,	/* track skew factor (LSB) */
		fmt_csfh,	/* cylinder skew factor (MSB) */
		fmt_csfl,	/* cylinder skew factor (LSB) */
		fmt_flags,	/* flags, see below */
		fmt_xxx[3];	/* reserved */
};
/* fmt_flags. Note, HSEC|SSEC meaning varies all over the map! */
#define	SCSI_FMT_HSEC	0x80	/* hard sector */
#define	SCSI_FMT_SSEC	0x40	/* soft sector */
#define	SCSI_FMT_RMB	0x20	/* removable media */
#define	SCSI_FMT_SURF	0x10	/* format by surface (vs. by cylinder) */
/*			0x0f	   reserved */

/*
 * Structure of a Rigid Disk Geometry mode page.
 * N.B.: CDC Wren V, at least, does not include rpm.
 */
struct scsi_page_rdgeom {
	u_char	rd_ncylh,	/* number of cylinders (MSB) */
		rd_ncylm,	/* number of cylinders */
		rd_ncyll,	/* number of cylinders (LSB) */
		rd_nheads,	/* number of heads */
		rd_wpcylh,	/* start cyl for write precomp. (MSB) */
		rd_wpcylm,	/* start cyl for write precomp. */
		rd_wpcyll,	/* start cyl for write precomp. (LSB) */
		rd_rwcylh,	/* start cyl for reduced write current (MSB) */
		rd_rwcylm,	/* start cyl for reduced write current */
		rd_rwcyll,	/* start cyl for reduced write current (LSB) */
		rd_steph,	/* drive step rate (.1 us units) (MSB) */
		rd_stepl,	/* drive step rate (LSB) */
		rd_lcylh,	/* landing zone cylinder (MSB) */
		rd_lcylm,	/* landing zone cylinder */
		rd_lcyll,	/* landing zone cylinder (LSB) */
		rd_rpl,		/* spindle synch control, see below */
		rd_roff,	/* rotational offset (for rpl) */
		rd_xxx1,	/* reserved */
		rd_rpmh,	/* medium rotation rate (rpm) (MSB) */
		rd_rpml,	/* medium rotation rate (rpm) (LSB) */
		rd_xxx2[2];	/* reserved */
};
/* values for rd_rpl. */
#define	SCSI_RD_RPL_MASK	0x03	/* mask for RPL field */
#define	SCSI_RD_RPL_NONE	0x00	/* sync disabled or not supported */
#define	SCSI_RD_RPL_SLAVE	0x01	/* disk is a Slave */
#define	SCSI_RD_RPL_MASTER	0x02	/* disk is a Master */
#define	SCSI_RD_RPL_MCONTROL	0x03	/* disk is a Master Control */

/*
 * Structure of a Verify Error Recovery mode page.
 */
struct scsi_page_verrrec {
	u_char	v_flags,	/* flags, see below */
		v_verify_retry,	/* verify retry count */
		v_corr_span,	/* verify correction span */
		v_xxx[5],	/* reserved */
		v_rtlh,		/* verify recovery time limit (MSB) */
		v_rtll;		/* verify recovery time limit (LSB) */
};
#define	SCSI_V_EER	0x08	/* use most expedient recovery, not best */
#define	SCSI_V_PER	0x04	/* report recovered errors */
#define	SCSI_V_DTE	0x02	/* stop after recovered error */
#define	SCSI_V_DCR	0x01	/* use ECC for detection only */

/*
 * Structure of a Caching mode page.
 */
struct scsi_page_cache {
	u_char	cache_flags,	/* flags, see below */
		cache_reten,	/* cache retention priorities (rd + wr) */
		cache_dptlh,	/* disable prefetch transfer length (MSB) */
		cache_dptll,	/* disable prefetch transfer length (LSB) */
		cache_minpfh,	/* minimum prefetch (MSB) */
		cache_minpfl,	/* minimum prefetch (LSB) */
		cache_maxpfh,	/* maximum prefetch (MSB) */
		cache_maxpfl,	/* maximum prefetch (LSB) */
		cache_mpch,	/* maximum prefetch ceiling (MSB) */
		cache_mpcl;	/* maximum prefetch ceiling (LSB) */
};
#define	SCSI_CACHE_WCE	0x04	/* write cache enable */
#define	SCSI_CACHE_MF	0x02	/* if set, prefetch depends on xfer length */
#define	SCSI_CACHE_RCD	0x01	/* read cache disable */

#define	SCSI_CACHE_RDPOLICY(x) ((x) >> 4)
#define	SCSI_CACHE_WRPOLICY(x) ((x) & 0xf)
#define	SCSI_CACHE_DEFAULT	0	/* use target default */
#define	SCSI_CACHE_KEEPPF	1	/* keep prefetch data over cmd data */
#define	SCSI_CACHE_KEEPCMD	15	/* keep cmd data over prefetch data */

/*
 * Structure of a Control Mode mode page.
 */
struct scsi_page_ctlmode {
	u_char	cm_rlec,	/* report log-activity exception condition */
		cm_qctl,	/* queue control (below) */
		cm_ecaaen,	/* ECA and AEN flags (below) */
		cm_xxx,		/* reserved */
		cm_aenholdh,	/* AEN holdoff period (ms) (MSB) */
		cm_aenholdl;	/* AEN holdoff period (ms) (LSB) */
};
#define	SCSI_CM_RLEC	0x01	/* RLEC flag occupies only low bit */
#define	SCSI_CM_QMOD(x) ((x) >> 4)	/* queue algorithm modifier */
#define	SCSI_CM_QERR	0x02		/* abort cmd queue after error */
#define	SCSI_CM_DQUE	0x01		/* disable tagged queueing */
#define	SCSI_CM_ECA	0x80	/* enable Extended Contingent Alliance */
#define	SCSI_CM_RAENP	0x04	/* target may do Async Err Notif after init */
#define	SCSI_CM_UAAENP	0x02	/* target may do AEN for Unit Attention */
#define	SCSI_CM_EAENP	0x01	/* target may do AEN for deferred errors */

/*
 * Structure of a CDC-specific Cache Control mode page.
 */
struct scsi_page_CDCcachectlmode {
	u_char	ccm_flags,	/* flags (below) */
		ccm_pfthresh,	/* prefetch threshold */
		ccm_maxthresh,	/* maximum threshold (?) */
		ccm_maxpfmult,	/* maximum prefetch multiplier */
		ccm_minthresh,	/* minimum thresold (?) */
		ccm_minpfmult,	/* minimum prefetch multiplier */
		ccm_xxx[8];	/* reserved */
};
#define	SCSI_CDC_CCM_WIE 0x40	/* write index enable */
#define	SCSI_CDC_CCM_CE	 0x10	/* cache enable */
#define	SCSI_CDC_CCM_TBLSZ(x) ((x) & 0xf) /* table size */

/*
 * Bits in cdb_lenl for a READ CAPACITY command,
 * and structure returned as data.
 *
 * If PMI is off, the lba in the cdb must be 0.
 */
#define	SCSI_CMD_RC_PMI		0x01	/* Partial Medium Indicator */

struct scsi_rc {
	u_char	rc_lbah;		/* logical block address (MSB) */
	u_char	rc_lbahm;		/* logical block address */
	u_char	rc_lbalm;		/* logical block address */
	u_char	rc_lbal;		/* logical block address (LSB) */
	u_char	rc_blh;			/* block length (MSB) */
	u_char	rc_blhm;		/* block length */
	u_char	rc_bllm;		/* block length */
	u_char	rc_bll;			/* block length (LSB) */
};
