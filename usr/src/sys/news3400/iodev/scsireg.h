/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: scsireg.h,v 4.300 91/06/09 06:38:12 root Rel41 $ SONY
 *
 *	@(#)scsireg.h	8.1 (Berkeley) 6/11/93
 */

/*
 *	scsireg.h
 */

#ifndef __SCSIREG__
#define __SCSIREG__ 1

/*
 *	initiator status byte bit image
 */
#define	INST_EP		0x80		/* End of Process */
#define INST_WR		0x40		/* Waiting Reselection */
#define	INST_IP		0x20		/* In Process */
#define	INST_WAIT	0x10		/* Waiting Bus free */
#define	INST_LB		0x8		/* Loss of BUSY */
#define	INST_TO		0x4		/* Time Out */
#define	INST_PRE	0x2		/* PaRameter Error */
#define	INST_HE		0x1		/* Hard Error */

#define	INSTERMASK	0x7


/*
 *	target status byte bit image
 */
#define	VENDOR		0x61
#define	TGSTMASK	0x1e
#define	TGST_RSVCFLCT	0x18
#define	TGST_INTERMED	0x10
#define	TGST_BUSY	0x8
#define	TGST_CC		0x2
#define	TGST_GOOD	0x0

#define	TS_MAPPED_PIO	0x01		/* program I/O with map */
#define	TS_CONTR_ON	0x02		/* contiguous transfer on */
#define	TS_CONTR_OFF	0x04		/* contiguous transfer off */
#define	TS_BYTE_DMA	0x08		/* DMA transfer(byte access) */
#define	TS_LONG_DMA	0x10		/* DMA transfer(long access) */


/*
 *	message byte
 */
#define MSG_IDENT	0x80
#define MSG_RESELEN	0x40
#define MSG_CCOMP	0
#define	MSG_EXTND	1
#define MSG_SDP		2
#define MSG_RDP		3
#define MSG_DCNT	4
#define MSG_IDE		5
#define MSG_ABORT	6
#define MSG_MREJ	7
#define MSG_NOP		8
#define MSG_PERROR	9


/*
 *	message identify byte bit image
 */
#define	IDT_DISCON	0x40
#define	IDT_DRMASK	0x7


/*
 *	scsi command opcodes
 */
#define SCOP_TST	0x00
#define SCOP_REZERO	0x01
#define	SCOP_REWIND	0x01
#define SCOP_RSENSE	0x03
#define SCOP_FMT	0x04
#define SCOP_RBLIM	0x05
#define SCOP_SPARAM	0x06
#define SCOP_RASBLK	0x07
#define SCOP_READ	0x08
#define SCOP_MOERASE	0x09
#define SCOP_WRITE	0x0a
#define SCOP_SEEK	0x0b
#define	SCOP_MERASE	0x0e
#define	SCOP_WFMARK	0x10
#define	SCOP_SPACE	0x11
#define SCOP_INQUIRY	0x12
#define	SCOP_SVERIFY	0x13
#define	SCOP_RBDATA	0x14
#define SCOP_MSELECT	0x15
#define	SCOP_ERASE	0x19
#define SCOP_MSENSE	0x1a
#define SCOP_STST	0x1b
#define	SCOP_LOAD	0x1b
#define SCOP_RECDIAG	0x1c
#define SCOP_SNDDIAG	0x1d
#define	SCOP_MEDRMV	0x1e
#define SCOP_RCAP	0x25
#define SCOP_EREAD	0x28
#define SCOP_EWRITE	0x2a
#define	SCOP_BSSRCH	0x2c
#define	SCOP_WSSRCH	0x2d
#define	SCOP_WRTVRFY	0x2e
#define SCOP_VERIFY	0x2f
#define SCOP_RDL	0x37
#define SCOP_WBUF	0x3b
#define SCOP_RBUF	0x3c
#define SCOP_EJECT	0xc0
#define SCOP_EESENSE	0xc1
#define SCOP_READTOC	0xc1
#define SCOP_READID	0xc2
#define SCOP_ADP	0xc2
#define SCOP_READQ	0xc2
#define SCOP_BLANKS	0xc3
#define SCOP_READHEAD	0xc3
#define SCOP_PBSTS	0xc4
#define SCOP_RCVDISK	0xc4
#define SCOP_PAUSE	0xc5
#define SCOP_PLAYTRACK	0xc6
#define SCOP_PLAYMSF	0xc7
#define SCOP_PLAYAUDIO	0xc8
#define SCOP_ERASED	0xe7
#define SCOP_RESET	0xff


/*
 *	other definition
 */
#define	ON	1
#define	OFF	0


/*
 *	scsi internal parameter block
 */
struct scsi {
/*00*/	u_char	sc_istatus;
/*01*/	u_char	sc_tstatus;
/*02*/	u_char	sc_identify;
/*03*/	u_char	sc_message;
/*04*/	u_int	sc_mpages;
/*08*/	u_int	sc_bytesec;
/*0c*/	u_char	*sc_cpoint;
/*10*/	u_int	sc_ctrnscnt;
/*14*/	struct sc_map *sc_map;
	union {
		struct	un_type0 {
/*18*/			u_int		t0_opcode : 8;
/*19*/			u_int		t0_lun	  : 3;
/*19*/			u_int		t0_lad    : 21;
/*1c*/			u_char		t0_count;
/*1d*/			u_char		t0_ctrl;
/*1e*/
		} un_type0;
		struct	un_tuio {
/*18*/			u_char		tu_opcode;
/*19*/			u_char		tu_lun	  : 3;
/*19*/			u_char		tu_resved : 3;
/*19*/			u_char		tu_code	  : 2;
/*1a*/			u_char		tu_count1;
/*1b*/			u_char		tu_count2;
/*1c*/			u_char		tu_count3;
/*1d*/			u_char		tu_ctrl;
/*1e*/
		} un_tuio;
		struct	un_mtio {
/*18*/			u_char		mt_opcode;
/*19*/			u_char		mt_lun	  : 3;
/*19*/			u_char		mt_resvd  : 2;
/*19*/			u_char		mt_st	  : 1;
/*19*/			u_char		mt_code	  : 2;
/*1a*/			u_char		mt_len1;
/*1b*/			u_char		mt_len2;
/*1c*/			u_char		mt_len3;
/*1d*/			u_char		mt_ctrl;
/*1e*/
		} un_mtio;
		struct	un_type1 {
/*18*/			u_char		t1_opcode;
/*19*/			u_char		t1_lun    : 3;
/*19*/			u_char		t1_rsvd   : 4;
/*19*/			u_char		t1_relat  : 1;
/*1a*/			u_short		t1_ladhi;
/*1c*/			u_short		t1_ladlo;
/*1e*/			u_char		t1_p1;
/*1f*/			u_char		t1_p2;
/*20*/			u_char		t1_p3;
/*21*/			u_char		t1_ctrl;
/*22*/
		} un_type1;
/*18*/		u_char	un_reserved[12];
/*24*/
	} sc_cdb;
/*24*/	u_char sc_param[20];	
/*38*/	int	sc_hbinfo;		/* Copy of the hb_ctlr->hm_hbinfo */
/*3c*/	u_int	sc_ctag;
/*40*/	u_int	sc_coffset;
/*44*/
};


#define	sc_opcode	sc_cdb.un_type0.t0_opcode
#define	sc_lun		sc_cdb.un_type0.t0_lun
#define	sc_lad		sc_cdb.un_type0.t0_lad
#define	sc_count	sc_cdb.un_type0.t0_count
#define	sc_nsect	sc_cdb.un_type0.t0_count
#define	sc_switch	sc_cdb.un_type0.t0_count
#define	sc_ctrl		sc_cdb.un_type0.t0_ctrl

#define	sc_tucode	sc_cdb.un_tuio.tu_code
#define	sc_tucount1	sc_cdb.un_tuio.tu_count1
#define	sc_tucount2	sc_cdb.un_tuio.tu_count2
#define	sc_tucount3	sc_cdb.un_tuio.tu_count3
#define	sc_tunsect1	sc_cdb.un_tuio.tu_count1
#define	sc_tunsect2	sc_cdb.un_tuio.tu_count2
#define	sc_tunsect3	sc_cdb.un_tuio.tu_count3

#define	sc_mtst		sc_cdb.un_mtio.mt_st
#define	sc_mtcode	sc_cdb.un_mtio.mt_code
#define	sc_mtlen1	sc_cdb.un_mtio.mt_len1
#define	sc_mtlen2	sc_cdb.un_mtio.mt_len2
#define	sc_mtlen3	sc_cdb.un_mtio.mt_len3
#define	sc_mtcount1	sc_cdb.un_mtio.mt_len1
#define	sc_mtcount2	sc_cdb.un_mtio.mt_len2
#define	sc_mtcount3	sc_cdb.un_mtio.mt_len3
#define	sc_mtnsect1	sc_cdb.un_mtio.mt_len1
#define	sc_mtnsect2	sc_cdb.un_mtio.mt_len2
#define	sc_mtnsect3	sc_cdb.un_mtio.mt_len3
#define	sc_mtctrl	sc_cdb.un_mtio.mt_ctrl
#define	sc_mtfxd	sc_mtcode
#define	sc_mtimm	sc_mtcode
#define	sc_mtlng	sc_mtcode

#define	sc_ladhi	sc_cdb.un_type1.t1_ladhi
#define	sc_ladlo	sc_cdb.un_type1.t1_ladlo
#define	sc_pmi		sc_cdb.un_type1.t1_p3

#define	scop_load(a,b,c,d,e)	scop_stst(a,b,c,d,e)


/*
 *	tape unit space operation code definitions
 */
#define	SCSC_DATA	0
#define	SCSC_FM		1
#define	SCSC_SQFM	2
#define	SCSC_EOD	3


/*
 *	scsi map table format
 */
#ifdef news3400
#define	NSCMAP	120
#endif

#ifdef news3800
#define	NSCMAP	129
#endif

struct sc_map {
/*000*/	unsigned	mp_offset;
/*004*/	unsigned	mp_pages;
/*008*/	unsigned	mp_addr[NSCMAP];
};


/*
 *	scsi nonextended sense data
 */
struct sc_nextnd {
/*00*/	u_int		scn_advalid : 1;
/*00*/	u_int		scn_ecode   : 7;
/*01*/	u_int		scn_resvd   : 3;
/*01*/	u_int		scn_secno   : 21;
/*04*/
};


/*
 *	scsi extended sense data
 */
struct sc_extnd {
/*00*/	u_char		sce_advalid : 1;
/*00*/	u_char		sce_extend  : 7;
/*01*/	u_char		sce_segno;
/*02*/	u_char		sce_fm	    : 1;
/*02*/	u_char		sce_eom	    : 1;
/*02*/	u_char		sce_ili	    : 1;
/*02*/	u_char		sce_resvd   : 1;
/*02*/	u_char		sce_skey    : 4;
/*03*/	u_char		sce_infob1;
/*04*/	u_char		sce_infob2;
/*05*/	u_char		sce_infob3;
/*06*/	u_char		sce_infob4;
/*07*/	u_char		sce_addlen;
	union {
		struct un_ehd {
/*08*/			u_short		ehd_resvd1;
/*0a*/			u_short		ehd_resvd2;
/*0c*/			u_char		ehd_ecode;
/*0d*/			u_char		ehd_resvd3;
/*0e*/			u_char		ehd_fru;
/*0f*/			u_char		ehd_fpv	   : 1;
/*0f*/			u_char		ehd_cd	   : 1;
/*0f*/			u_char		ehd_resvd4 : 2;
/*0f*/			u_char		ehd_bpv	   : 1;
/*0f*/			u_char		ehd_bitpnt : 3;
/*10*/			u_short		ehd_fldpnt;
/*12*/
		} un_ehd;
		struct un_etu {
/*08*/			u_char		etu_ecode;
/*09*/			u_char		etu_nerrhi;
/*0a*/			u_char		etu_nerrlo;
/*0b*/
		} un_etu;
		struct un_emt {
/*08*/			u_short		emt_estat;
/*0a*/			u_char		emt_resvd1;
/*0b*/			u_char		emt_totlrtry;
/*0c*/			u_short		emt_resvd2;
/*0e*/			u_short		emt_resvd3;
/*10*/			u_char		emt_resvd4;
/*11*/			u_char		emt_ecode;
/*12*/
		} un_emt;
		struct un_ewo {
/*08*/			u_char		ewo_resvd1;
/*09*/			u_char		ewo_resvd2;
/*0a*/			u_char		ewo_resvd3;
/*0b*/			u_char		ewo_sadvalid : 1;
/*0b*/			u_char		ewo_secode   : 7;
/*0c*/			u_char		ewo_saddr1;
/*0d*/			u_char		ewo_saddr2;
/*0e*/			u_char		ewo_saddr3;
/*0f*/			u_char		ewo_saddr4;
/*10*/			u_char		ewo_resvd4;
/*11*/			u_char		ewo_dadvalid : 1;
/*11*/			u_char		ewo_decode   : 7;
/*12*/			u_char		ewo_daddr1;
/*13*/			u_char		ewo_daddr2;
/*14*/			u_char		ewo_daddr3;
/*15*/			u_char		ewo_daddr4;
/*16*/
		} un_ewo;
		struct un_eod {
/*08*/			u_char		eod_resvd1;
/*09*/			u_char		eod_resvd2;
/*0a*/			u_char		eod_resvd3;
/*0b*/			u_char		eod_resvd4;
/*0c*/			u_char		eod_ecode;
/*0d*/			u_char		eod_resvd5;
/*0e*/			u_char		eod_resvd6;
/*0f*/			u_char		eod_resvd7;
/*10*/			u_char		eod_resvd8;
/*11*/			u_char		eod_resvd9;
/*12*/
		} un_eod;
/*08*/		u_char un_data[24];
/*20*/
	} sce_add;
/*20*/
};

#define	sce_hdecode	sce_add.un_ehd.ehd_ecode
 
#define	sce_tuecode	sce_add.un_etu.etu_ecode
#define	sce_tunerrhi	sce_add.un_etu.etu_nerrhi
#define	sce_tunerrlo	sce_add.un_etu.etu_nerrlo

#define	sce_mtestat	sce_add.un_emt.emt_estat
#define	sce_mtecode	sce_add.un_emt.emt_ecode

#define	sce_odecode	sce_add.un_eod.eod_ecode

#define	sce_ascq	sce_add.un_ehd.ehd_resvd3
#define	sce_sksv	sce_add.un_ehd.ehd_fpv
#define	sce_actretry	sce_add.un_ehd.ehd_fldpnt

/*
 *	scsi inquiry response data
 */
struct sc_inq {
/*00*/	u_char		sci_devtype;
/*01*/	u_char		sci_qual;
/*02*/	u_char		sci_version;
/*03*/	u_char		sci_resvd1;
/*04*/	u_char		sci_ninfo;
/*05*/	u_char		sci_drinfo;
/*06*/	u_char		sci_firmrev;
/*07*/	u_char		sci_ready;
/*08*/	u_char		sci_vendid[8];
/*10*/	u_char		sci_prodid[16];
/*20*/	u_char		sci_revision[4];
/*24*/
};


/*
 *	scsi read capacity data
 */
struct sc_rcap {
/*00*/	u_int		scr_nblock;
/*04*/	u_int		scr_blocklen;
/*08*/
};


/*
 *	scsi mode sense/select data
 */
struct sc_mdata {
/*00*/	u_char	scm_len;
/*01*/	u_char	scm_type;
/*02*/	u_char	scm_flags1;
/*03*/	u_char	scm_dlen;
/*04*/	u_int	scm_dens	: 8;
/*05*/	u_int	scm_nblock	: 24;
/*08*/	u_int	scm_resvd1	: 8;
/*09*/	u_int	scm_bsize	: 24;
/*0c*/	u_char	scm_flags2;
/*0d*/	u_char	scm_resvd2;
/*0e*/	u_char	scm_resvd3;
/*0f*/	u_char	scm_maxrtry;
/*10*/
};

#define	scm_tdens	scm_flags2


/*
 *	bits of scm_flags1
 */
#define	SCM1_WRP	0x80
#define	SCM1_BUFM	0x10
#define	SCM1_SPD90	0x02


/*
 *	scm_type
 */
#define	SCMT_DEFAULT	0x0
#define	SCMT_150_600	0x80
#define	SCMT_300_450	0x81


/*
 *	scm_dens
 */
#define	SCMD_QIC_24_9	0x0	/* This may be 0x5 */
#define	SCMD_QIC_11_4	0x4
#define	SCMD_QIC_11_9	0x84
#define	SCMD_QIC_120_15	0xf
#define	SCMD_QIC_150_18	0x10

#define	SCMD_DEFAULT	0x00
#define	SCMD_800_BPI	0x01
#define	SCMD_1600_BPI	0x02
#define	SCMD_6250_BPI	0x03
#define	SCMD_3200_BPI	0x06
#define	SCMD_NEWTAPE	0x80
#define	SCMD_NOTAPE	0xff


/*
 *	bits of scm_flags2
 */
#define	SCM2_DEA	0x04
#define	SCM2_AUI	0x02
#define	SCM2_SEC	0x01


/*
 *	scsi reassign block perameter list
 */
struct sc_rab {
/*00*/	u_short	sca_resved;
/*02*/	u_short	sca_dllen;
/*04*/	u_int	sca_dlad[4];
/*14*/
};


#ifdef CPU_DOUBLE

# ifdef mips
#  define	ipc_phys(x)	(caddr_t)K0_TT0(x)
# else
#  define	ipc_phys(x)	(caddr_t)((int)(x) & ~0x80000000)
# endif

# ifdef news3800
#  define	splsc		spl4
#  define	splscon		spl3
# endif

#endif /* CPU_DOUBLE */

#ifdef CPU_SINGLE
# define	ipc_phys(x)	(caddr_t)(x)
# ifdef news3400
#  define	splsc		spl3
#  define	splscon		spl2
# else
#  define	splsc		spl4
#  define	splscon		spl3
# endif
#endif /* CPU_SINGLE */

#define	SCSI_INTEN	1
#define	SCSI_INTDIS	0

struct scintsw {
/*00*/	int	(*sci_inthandler)();	/* pointer to interrupt handler */
/*04*/	int	sci_ctlr;		/* controller number */
/*08*/
};

struct sc_data {
/*00*/	caddr_t	scd_scaddr;		/* pointer to struct scsi */
/*04*/	caddr_t	scd_vaddr;		/* pointer to buffer address */
/*08*/	int	scd_count;		/* buffer size */
/*0c*/	int	scd_rw;			/* R/W flag see buf.h */
/*10*/	struct proc *scd_procp;		/* user prrocess */
/*14*/
};

#endif /* !__SCSIREG__ */
