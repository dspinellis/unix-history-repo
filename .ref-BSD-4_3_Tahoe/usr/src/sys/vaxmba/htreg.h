/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)htreg.h	7.2 (Berkeley) 5/12/87
 */

struct	htdevice
{
	int	htcs1;		/* control status register */
	int	htds;		/* drive status register */
	int	hter;		/* error register */
	int	htmr;		/* maintenance register */
	int	htas;		/* attention status register */
	int	htfc;		/* frame counter */
	int	htdt;		/* drive type register */
	int	htck;		/* nrzi check (crc) error character */
	int	htsn;		/* serial number register */
	int	httc;		/* tape controll register */
};

/* htcs1 */
#define	HT_GO		000001		/* go bit */
#define	HT_SENSE	000000		/* no operations (sense) */
#define	HT_REWOFFL	000002		/* rewind offline */
#define	HT_REW		000006		/* rewind */
#define	HT_DCLR		000010		/* drive clear */
#define	HT_RIP		000020		/* read in preset */
#define	HT_ERASE	000024		/* erase */
#define	HT_WEOF		000026		/* write tape mark */
#define	HT_SFORW	000030		/* space forward */
#define	HT_SREV		000032		/* space reverse */
#define	HT_WCHFWD	000050		/* write check forward */
#define	HT_WCHREV	000056		/* write check reverse */
#define	HT_WCOM		000060		/* write forward */
#define	HT_RCOM		000070		/* read forward */
#define	HT_RREV		000076		/* read reverse */

/* htds */
#define	HTDS_ATA	0100000		/* attention active */
#define	HTDS_ERR	0040000		/* composite error */
#define	HTDS_PIP	0020000		/* positioning in progress */
#define	HTDS_MOL	0010000		/* medium on line */
#define	HTDS_WRL	0004000		/* write lock */
#define	HTDS_EOT	0002000		/* end of tape */
/* bit 9 is unused */
#define	HTDS_DPR	0000400		/* drive present (always 1) */
#define	HTDS_DRY	0000200		/* drive ready */
#define	HTDS_SSC	0000100		/* slave status change */
#define	HTDS_PES	0000040		/* phase-encoded status */
#define	HTDS_SDWN	0000020		/* settle down */
#define	HTDS_IDB	0000010		/* identification burst */
#define	HTDS_TM		0000004		/* tape mark */
#define	HTDS_BOT	0000002		/* beginning of tape */
#define	HTDS_SLA	0000001		/* slave attention */

#define	HTDS_BITS \
"\10\20ATA\17ERR\16PIP\15MOL\14WRL\13EOT\11DPR\10DRY\
\7SSC\6PES\5SDWN\4IDB\3TM\2BOT\1SLA"

/* hter */
#define	HTER_CORCRC	0100000		/* correctible data or ecc */
#define	HTER_UNS	0040000		/* unsafe */
#define	HTER_OPI	0020000		/* operation incomplete */
#define	HTER_DTE	0010000		/* drive timing error */
#define	HTER_NEF	0004000		/* non-executable function */
#define	HTER_CSITM	0002000		/* correctable skew/illegal tape mark */
#define	HTER_FCE	0001000		/* frame count error */
#define	HTER_NSG	0000400		/* non-standard gap */
#define	HTER_PEFLRC	0000200		/* format error or lrc error */
#define	HTER_INCVPE	0000100		/* incorrectable data error or vertical
					   parity error */
#define	HTER_DPAR	0000040		/* data parity error */
#define	HTER_FMT	0000020		/* format error */
#define	HTER_CPAR	0000010		/* control bus parity error */
#define	HTER_RMR	0000004		/* register modification refused */
#define	HTER_ILR	0000002		/* illegal register */
#define	HTER_ILF	0000001		/* illegal function */

#define	HTER_BITS \
"\10\20CORCRC\17UNS\16OPI\15DTE\14NEF\13CSITM\12FCE\11NSG\10PEFLRC\
\7INCVPE\6DPAR\5FMT\4CPAR\3RMR\2ILR\1ILF"
#define	HTER_HARD \
	(HTER_UNS|HTER_OPI|HTER_NEF|HTER_DPAR|HTER_FMT|HTER_CPAR| \
	HTER_RMR|HTER_ILR|HTER_ILF)

/* htdt */
#define	HTDT_NSA	0100000		/* not sector addressed; always 1 */
#define	HTDT_TAP	0040000		/* tape; always 1 */
#define	HTDT_MOH	0020000		/* moving head; always 0 */
#define	HTDT_7CH	0010000		/* 7 channel; always 0 */
#define	HTDT_DRQ	0004000		/* drive requested; always 0 */
#define	HTDT_SPR	0002000		/* slave present */
/* bit 9 is spare */
/* bits 8-0 are formatter/transport type */

/* httc */
#define	HTTC_ACCL	0100000		/* transport is not reading/writing */
#define	HTTC_FCS	0040000		/* frame count status */
#define	HTTC_SAC	0020000		/* slave address change */
#define	HTTC_EAODTE	0010000		/* enable abort on data xfer errors */
/* bits 8-10 are density select */
#define	HTTC_800BPI	0001400		/* in bits 8-10, dens=1600 */
#define	HTTC_1600BPI	0002000		/* in bits 8-10, dens=800 */
#define	HTTC_6250BPI	0003400		/* in bits 8-10, dens=6250 */
/* bits 4-7 are format select */
#define	HTTC_PDP11	0000300		/* in bits 4-7, pdp11 normal format */
#define	HTTC_EVEN	0000010		/* select even parity */
/* bits 0 - 2 are slave select */

#define	b_repcnt  b_bcount
#define	b_command b_resid
