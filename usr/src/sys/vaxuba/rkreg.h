/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rkreg.h	7.1 (Berkeley) 6/5/86
 */

#define NRK7CYL 	815
#define	NRK6CYL		411
#define NRKSECT		22
#define NRKTRK		3

struct rkdevice
{
	short	rkcs1;		/* control status reg 1 */
	short	rkwc;		/* word count */
	u_short	rkba;		/* bus address */
	short	rkda;		/* disk address */
	short	rkcs2;		/* control status reg 2 */
	short	rkds;		/* drive status */
	short	rker;		/* driver error register */
	short	rkatt;		/* attention status/offset register */
	short	rkcyl;		/* current cylinder register */
	short	rkxxx;
	short	rkdb;		/* data buffer register */
	short	rkmr1;		/* maint reg 1 */
	short	rkec1;		/* burst error bit position */
	short	rkec2;		/* burst error bit pattern */
	short	rkmr2;		/* maint reg 2 */
	short	rkmr3;		/* maint reg 3 */
};

/* rkcs1 */
#define RK_CCLR		0100000		/* controller clear (also error) */
#define	RK_CERR		RK_CCLR
#define	RK_DI		0040000		/* drive interrupt */
#define	RK_DTCPAR	0020000		/* drive to controller parity */
#define	RK_CFMT		0010000		/* 18 bit word format */
#define	RK_CTO		0004000		/* controller timeout */
#define	RK_CDT		0002000		/* drive type (rk07/rk06) */
/* bits 8 and 9 are the extended bus address */
#define	RK_CRDY		0000200		/* controller ready */
#define	RK_IE		0000100		/* interrupt enable */
/* bits 1 to 4 are the function code */
#define	RK_GO		0000001

/* commands */
#define RK_SELECT	000		/* select drive */
#define RK_PACK		002		/* pack acknowledge */
#define RK_DCLR		004		/* drive clear */
#define	RK_UNLOAD	006		/* unload */
#define	RK_START	010		/* start spindle */
#define	RK_RECAL	012		/* recalibrate */
#define	RK_OFFSET	014		/* offset */
#define	RK_SEEK		016		/* seek */
#define	RK_READ		020		/* read data */
#define	RK_WRITE	022		/* write data */
#define	RK_RHDR		026		/* read header */
#define	RK_WHDR		030		/* write header */

/* rkcs2 */
#define	RKCS2_DLT	0100000		/* data late */
#define	RKCS2_WCE	0040000		/* write check */
#define	RKCS2_UPE	0020000		/* unibus parity */
#define	RKCS2_NED	0010000		/* non-existant drive */
#define	RKCS2_NEM	0004000		/* non-existant memory */
#define	RKCS2_PGE	0002000		/* programming error */
#define	RKCS2_MDS	0001000		/* multiple drive select */
#define	RKCS2_UFE	0000400		/* unit field error */
#define	RKCS2_OR	0000200		/* output ready */
#define	RKCS2_IR	0000100		/* input ready */
#define	RKCS2_SCLR	0000040		/* subsystem clear */
#define	RKCS2_BAI	0000020		/* bus address increment inhibit */
#define	RKCS2_RLS	0000010		/* release */
/* bits 0-2 are drive select */

#define	RKCS2_BITS \
"\10\20DLT\17WCE\16UPE\15NED\14NEM\13PGE\12MDS\11UFE\
\10OR\7IR\6SCLR\5BAI\4RLS"

#define	RKCS2_HARD		(RKCS2_NED|RKCS2_PGE)

/* rkds */
#define	RKDS_SVAL	0100000		/* status valid */
#define	RKDS_CDA	0040000		/* current drive attention */
#define	RKDS_PIP	0020000		/* positioning in progress */
/* bit 12 is spare */
#define	RKDS_WRL	0004000		/* write lock */
/* bits 9 and 10 are spare */
#define	RKDS_DDT	0000400		/* disk drive type */
#define	RKDS_DRDY	0000200		/* drive ready */
#define	RKDS_VV		0000100		/* volume valid */
#define	RKDS_DROT	0000040		/* drive off track */
#define	RKDS_SPLS	0000020		/* speed loss */
#define	RKDS_ACLO	0000010		/* ac low */
#define	RKDS_OFF	0000004		/* offset mode */
#define	RKDS_DRA	0000001		/* drive available */

#define	RKDS_DREADY	(RKDS_DRA|RKDS_VV|RKDS_DRDY)
#define	RKDS_BITS \
"\10\20SVAL\17CDA\16PIP\14WRL\11DDT\
\10DRDY\7VV\6DROT\5SPLS\4ACLO\3OFF\1DRA"
#define	RKDS_HARD	(RKDS_ACLO|RKDS_SPLS)

/* rker */
#define	RKER_DCK	0100000		/* data check */
#define	RKER_UNS	0040000		/* drive unsafe */
#define	RKER_OPI	0020000		/* operation incomplete */
#define	RKER_DTE	0010000		/* drive timing error */
#define	RKER_WLE	0004000		/* write lock error */
#define	RKER_IDAE	0002000		/* invalid disk address error */
#define	RKER_COE	0001000		/* cylinder overflow error */
#define	RKER_HRVC	0000400		/* header vertical redundancy check */
#define	RKER_BSE	0000200		/* bad sector error */
#define	RKER_ECH	0000100		/* hard ecc error */
#define	RKER_DTYE	0000040		/* drive type error */
#define	RKER_FMTE	0000020		/* format error */
#define	RKER_DRPAR	0000010		/* control-to-drive parity error */
#define	RKER_NXF	0000004		/* non-executable function */
#define	RKER_SKI	0000002		/* seek incomplete */
#define	RKER_ILF		0000001		/* illegal function */

#define	RKER_BITS \
"\10\20DCK\17UNS\16OPI\15DTE\14WLE\13IDAE\12COE\11HRVC\
\10BSE\7ECH\6DTYE\5FMTE\4DRPAR\3NXF\2SKI\1ILF"
#define	RKER_HARD	\
	(RKER_WLE|RKER_IDAE|RKER_COE|RKER_DTYE|RKER_FMTE|RKER_ILF)

/* offset bits in rkas */
#define	RKAS_P400	0020		/*  +400 RK06,  +200 RK07 */
#define	RKAS_M400	0220		/*  -400 RK06,  -200 RK07 */
#define	RKAS_P800	0040		/*  +800 RK06,  +400 RK07 */
#define	RKAS_M800	0240		/*  -800 RK06,  -400 RK07 */
#define	RKAS_P1200	0060		/*  +800 RK06,  +400 RK07 */
#define	RKAS_M1200	0260		/* -1200 RK06, -1200 RK07 */
