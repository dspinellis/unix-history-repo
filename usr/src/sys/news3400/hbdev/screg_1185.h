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
 * from: $Hdr: screg_1185.h,v 4.300 91/06/09 06:22:14 root Rel41 $ SONY
 *
 *	@(#)screg_1185.h	8.1 (Berkeley) 6/11/93
 */

/*
 * Copyright (c) 1989- by SONY Corporation.
 */

/*
 *	screg_1185.h	ver 0.0		
 *		for SCSI I/F Chip CXD1185Q
 */

/*
 *		SCSI I/F Chip CXD1185Q Register address assignment
 */
#ifdef mips
# define	SCSI_BASE	0xbfe00100
#else
# define	SCSI_BASE	0xe1900000
#endif

#ifndef U_CHAR
#ifdef mips
#define U_CHAR volatile u_char
#else
#define U_CHAR u_char
#endif
#endif

#define	sc_statr	*( (U_CHAR *)(SCSI_BASE + 0x0) )
#define	sc_comr		*( (U_CHAR *)(SCSI_BASE + 0x0) )
#define	sc_datr		*( (U_CHAR *)(SCSI_BASE + 0x1) )
#define	sc_intrq1	*( (U_CHAR *)(SCSI_BASE + 0x2) )
#define	sc_intrq2	*( (U_CHAR *)(SCSI_BASE + 0x3) )
#define	sc_envir	*( (U_CHAR *)(SCSI_BASE + 0x3) )
#define	sc_cmonr	*( (U_CHAR *)(SCSI_BASE + 0x4) )
#define	sc_timer	*( (U_CHAR *)(SCSI_BASE + 0x4) )
#define	sc_ffstr	*( (U_CHAR *)(SCSI_BASE + 0x5) )
#define	sc_idenr	*( (U_CHAR *)(SCSI_BASE + 0x6) )
#define	sc_tclow	*( (U_CHAR *)(SCSI_BASE + 0x7) )
#define	sc_tcmid	*( (U_CHAR *)(SCSI_BASE + 0x8) )
#define	sc_tchi		*( (U_CHAR *)(SCSI_BASE + 0x9) )
#define	sc_intok1	*( (U_CHAR *)(SCSI_BASE + 0xa) )
#define	sc_intok2	*( (U_CHAR *)(SCSI_BASE + 0xb) )
#define	sc_moder	*( (U_CHAR *)(SCSI_BASE + 0xc) )
#define	sc_syncr	*( (U_CHAR *)(SCSI_BASE + 0xd) )
#define	sc_busconr	*( (U_CHAR *)(SCSI_BASE + 0xe) )
#define	sc_ioptr	*( (U_CHAR *)(SCSI_BASE + 0xf) )

/*
 *		CXD1185Q Register bit assignment
 */

/*	sc_statr (status register) bit define
*/
#define	R0_MRST		0x80
#define	R0_MDBP		0x40
#define	R0_INIT		0x10
#define	R0_TARG		8
#define	R0_TRBZ		4
#define	R0_MIRQ		2
#define	R0_CIP		1

/*	sc_comr (command register) bit define
*/
#define	R0_DMA		0x20
#define	R0_TRBE		0x10

/*	sc_intrq1 (interrupt request register 1) bit define
*/
#define	R2_STO		0x10
#define	R2_RSL		8
#define	R2_SWA		4
#define	R2_SWOA		2
#define	R2_ARBF		1

/*	sc_intrq2 (interrupt request register 2) bit define
*/
#define	R3_FNC		0x80
#define	R3_DCNT		0x40
#define	R3_SRST		0x20
#define	R3_PHC		0x10
#define	R3_DATN		8
#define	R3_DPE		4
#define	R3_SPE		2
#define	R3_RMSG		1

/*	sc_envir (environment register) bit define
*/
#define	R3_DIFE		0x80
#define	R3_SDPM		0x40
#define	R3_DPEN		0x20
#define	R3_SIRM		0x10
#define	R3_FS_MASK	3

/*	sc_cmonr (scsi control monitor register) bit define
*/
#define	R4_MBSY		0x80
#define	R4_MSEL		0x40
#define	R4_MMSG		0x20
#define	R4_MCD		0x10
#define	R4_MIO		8
#define	R4_MREQ		4
#define	R4_MACK		2
#define	R4_MATN		1

/*	sc_ffstr (FIFO status register) bit define
*/
#define	R5_FIE		0x80
#define	R5_FIF		0x10
#define	R5_FIFOREM	0x1f

/*	sc_idenr (scsi identify register) bit define
*/
#define	R6_OID_MASK	0x07
#define	R6_SID_MASK	0xe0
#define	R6_TID_MASK	0xe0

/*	sc_intok1 (interrupt enable register 1) bit define
*/
#define	Ra_STO		0x10
#define	Ra_RSL		8
#define	Ra_SWA		4
#define	Ra_SWOA		2
#define	Ra_ARBF		1

/*	sc_intok2 (interrupt enable register 2) bit define
*/
#define	Rb_FNC		0x80
#define	Rb_DCNT		0x40
#define	Rb_SRST		0x20
#define	Rb_PHC		0x10
#define	Rb_DATN		8
#define	Rb_DPE		4
#define	Rb_SPE		2
#define	Rb_RMSG		1

/*	sc_moder (mode register) bit define
*/
#define	Rc_HDPE		0x80
#define	Rc_HSPE		0x40
#define	Rc_HATN		0x20
#define	Rc_TMSL		0x10
#define	Rc_SPHI		8
#define	Rc_BDMA		1

/*	sc_syncr (synchronous transfer control register) bit define
*/
#define	Rd_TPD_MASK	0xf0
#define	Rd_TOF_MASK	0x0f
#define	MIN_TP		62		/* minimum transfer period 4ns * 25 */
#define	MAX_OFFSET	15

/*	sc_busconr (scsi bus control register) bit define
*/
#define	Re_ABSY		0x80
#define	Re_ASEL		0x40
#define	Re_AMSG		0x20
#define	Re_ACD		0x10
#define	Re_AIO		8
#define	Re_AREQ		4
#define	Re_AACK		2
#define	Re_AATN		1

/*	sc_ioptr (I/O port) bit define
*/
#define	Rf_PCN_MASK	0xf0
# define	Rf_PCN3		0x80
# define	Rf_PCN2		0x40
# define	Rf_PCN1		0x20
# define	Rf_PCN0		0x10
#define	Rf_PRT_MASK	0x0f
# define	Rf_PRT3		8
# define	Rf_PRT2		4
# define	Rf_PRT1		2
# define	Rf_PRT0		1


/*
 *		CXD1185Q commands
 */
/*	category 0
*/
#define	SCMD_NOP	0x00
#define	SCMD_CHIP_RST	0x01
#define	SCMD_AST_RST	0x02
#define	SCMD_FLSH_FIFO	0x03
#define	SCMD_AST_CTRL	0x04
#define	SCMD_NGT_CTRL	0x05
#define	SCMD_AST_DATA	0x06
#define	SCMD_NGT_DATA	0x07

/*	category 1
*/
#define	SCMD_RESEL	0x40
#define	SCMD_SEL	0x41
#define	SCMD_SEL_ATN	0x42
#define	SCMD_ENB_SEL	0x43
#define	SCMD_DIS_SEL	0x44

/*	category 2
*/
#define	SCMD_SEND_MES	0x80
#define	SCMD_SEND_STAT	0x81
#define	SCMD_SEND_DATA	0x82
#define	SCMD_DISCONNECT	0x83
#define	SCMD_RCV_MOUT	0x84
#define	SCMD_RCV_CMD	0x85
#define	SCMD_RCV_DATA	0x86

/*	category 3
*/
#define	SCMD_TR_INFO	0xc0
#define	SCMD_TR_PAD	0xc1
#define	SCMD_NGT_ACK	0xc2
#define	SCMD_AST_ATN	0xc3
#define	SCMD_NGT_ATN	0xc4


/*
 *		scsi parameter definition
 */
/* 	SCSI bus ID
*/
#define	SC_OWNID	0x7
#define	SC_TG_SHIFT	5

/*	scsi bus phase
*/
#define	SC_PMASK		(R4_MMSG|R4_MCD|R4_MIO)
# define	DAT_OUT		0
# define	DAT_IN				R4_MIO
# define	COM_OUT			 R4_MCD
# define	STAT_IN			(R4_MCD|R4_MIO)
# define	MES_OUT		(R4_MMSG|R4_MCD)
# define	MES_IN		(R4_MMSG|R4_MCD|R4_MIO)

/*	scsi command types define
*/
#define	CMD_TYPEMASK	0xe0
# define	CMD_T0		0		/*  6 byte commands */
# define	CMD_T1		0x20		/* 10 byte commands */
# define	CMD_T5		0xa0		/* 12 byte commands */
# define	CMD_T6		0xc0
# define	CMD_T7		0xe0

#define MAXNSCSI	1
