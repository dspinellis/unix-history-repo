/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)upreg.h	7.1 (Berkeley) %G%
 */

/*
 * Unibus rm emulation via sc21:
 * registers and bits.
 */

struct updevice
{
	u_short	upcs1;		/* control and status register 1 */
	short	upwc;		/* word count register */
	u_short	upba;		/* UNIBUS address register */
	u_short	upda;		/* desired address register */
	u_short	upcs2;		/* control and status register 2 */
	u_short	upds;		/* drive Status */
	u_short	uper1;		/* error register 1 */
	u_short	upas;		/* attention summary */
	u_short	upla;		/* look ahead */
	u_short	updb;		/* data buffer */
	u_short	upmr;		/* maintenance */ 
	u_short	updt;		/* drive type */
	u_short	upsn;		/* serial number */
	u_short	upof;		/* offset register */
	u_short	updc;		/* desired cylinder address register */
	u_short	uphr;		/* holding register */
	u_short	upmr2;		/* maintenance register 2 */
	u_short	uper2;		/* error register 2 */
	u_short	upec1;		/* burst error bit position */
	u_short	upec2;		/* burst error bit pattern */
};

/* Other bits of upcs1 */
#define	UP_SC	0100000		/* special condition */
#define	UP_TRE	0040000		/* transfer error */
#define	UP_PSEL	0010000		/* port select */
#define	UP_DVA	0004000		/* drive available */
/* bits 8 and 9 are the extended address bits */
#define	UP_RDY	0000200		/* controller ready */
#define	UP_IE	0000100		/* interrupt enable */
/* bits 5-1 are the command */
#define	UP_GO	0000001

/* commands */
#define	UP_NOP		000
#define	UP_SEEK		004		/* seek */
#define	UP_RECAL	006		/* recalibrate */
#define	UP_DCLR		010		/* drive clear */
#define	UP_RELEASE	012		/* release */
#define	UP_OFFSET	014		/* offset */
#define	UP_RTC		016		/* return to center-line */
#define	UP_PRESET	020		/* read-in preset */
#define	UP_PACK		022		/* pack acknowledge */
#define	UP_DMABAND	024		/* dma bandwidth set */
#define	UP_SEARCH	030		/* search */
#define	UP_WCDATA	050		/* write check data */
#define	UP_WCHDR	052		/* write check header and data */
#define	UP_WCOM		060		/* write */
#define	UP_WHDR		062		/* write header and data */
#define	UP_RCOM		070		/* read data */
#define	UP_RHDR		072		/* read header and data */
#define	UP_BOOT		074		/* boot */
#define	UP_FORMAT	076		/* format */

/* upcs2 */
#define	UPCS2_DLT	0100000		/* data late */
#define	UPCS2_WCE	0040000		/* write check error */
#define	UPCS2_UPE	0020000		/* UNIBUS parity error */
#define	UPCS2_NED	0010000		/* nonexistent drive */
#define	UPCS2_NEM	0004000		/* nonexistent memory */
#define	UPCS2_PGE	0002000		/* programming error */
#define	UPCS2_MXF	0001000		/* missed transfer */
#define	UPCS2_MDPE	0000400		/* massbus data parity error (0) */
#define	UPCS2_OR	0000200		/* output ready */
#define	UPCS2_IR	0000100		/* input ready */
#define	UPCS2_CLR	0000040		/* controller clear */
#define	UPCS2_PAT	0000020		/* parity test */
#define	UPCS2_BAI	0000010		/* address increment inhibit */
/* bits 0-2 are drive select */

#define	UPCS2_BITS \
"\10\20DLT\17WCE\16UPE\15NED\14NEM\13PGE\12MXF\11MDPE\
\10OR\7IR\6CLR\5PAT\4BAI"

/* upds */
#define	UPDS_ATA	0100000		/* attention active */
#define	UPDS_ERR	0040000		/* composite drive error */
#define	UPDS_PIP	0020000		/* positioning in progress */
#define	UPDS_MOL	0010000		/* medium on line */
#define	UPDS_WRL	0004000		/* write locked */
#define	UPDS_LST	0002000		/* last sector transferred */
#define	UPDS_PGM	0001000		/* programmable */
#define	UPDS_DPR	0000400		/* drive present */
#define	UPDS_DRY	0000200		/* drive ready */
#define	UPDS_VV		0000100		/* volume valid */
/* bits 1-5 are spare */
#define	UPDS_OM		0000001		/* offset mode */

#define	UPDS_DREADY	(UPDS_DPR|UPDS_DRY|UPDS_MOL|UPDS_VV)

#define	UPDS_BITS \
"\10\20ATA\17ERR\16PIP\15MOL\14WRL\13LST\12PGM\11DPR\10DRY\7VV\1OM"

/* uper1 */
#define	UPER1_DCK	0100000		/* data check */
#define	UPER1_UNS	0040000		/* drive unsafe */
#define	UPER1_OPI	0020000		/* operation incomplete */
#define	UPER1_DTE	0010000		/* drive timing error */
#define	UPER1_WLE	0004000		/* write lock error */
#define	UPER1_IAE	0002000		/* invalid address error */
#define	UPER1_AOE	0001000		/* address overflow error */
#define	UPER1_HCRC	0000400		/* header crc error */
#define	UPER1_HCE	0000200		/* header compare error */
#define	UPER1_ECH	0000100		/* ecc hard error */
#define	UPER1_WCF	0000040		/* write clock fail (0) */
#define	UPER1_FER	0000020		/* format error */
#define	UPER1_PAR	0000010		/* parity error */
#define	UPER1_RMR	0000004		/* register modification refused */
#define	UPER1_ILR	0000002		/* illegal register */
#define	UPER1_ILF	0000001		/* illegal function */

#define	UPER1_BITS \
"\10\20DCK\17UNS\16OPI\15DTE\14WLE\13IAE\12AOE\11HCRC\10HCE\
\7ECH\6WCF\5FER\4PAR\3RMR\2ILR\1ILF"

/* uphr */
/* write these int uphr and then read back values */
#define	UPHR_MAXCYL	0100027		/* max cyl address */
#define	UPHR_MAXTRAK	0100030		/* max track address */
#define	UPHR_MAXSECT	0100031		/* max sector address */

/* uper2 */
#define	UPER2_BSE	0100000		/* bad sector error */
#define	UPER2_SKI	0040000		/* seek incomplete */
#define	UPER2_OPE	0020000		/* operator plug error */
#define	UPER2_IVC	0010000		/* invalid command */
#define	UPER2_LSC	0004000		/* loss of sector clock */
#define	UPER2_LBC	0002000		/* loss of bit clock */
#define	UPER2_MDS	0001000		/* multiple drive select */
#define	UPER2_DCU	0000400		/* dc power unsafe */
#define	UPER2_DVC	0000200		/* device check */
#define	UPER2_ACU	0000100		/* ac power unsafe */
/* bits 5 and 4 are spare */
#define	UPER2_DPE	0000010		/* data parity error (0) */
/* bits 2-0 are spare */

#define	UPER2_BITS \
"\10\20BSE\17SKI\16OPE\15IVC\14LSC\13LBC\12MDS\11DCU\10DVC\7ACU\4DPE"

/* upof */
#define	UPOF_FMT22	0010000		/* 16 bit format */
#define	UPOF_ECI	0004000		/* ecc inhibit */
#define	UPOF_HCI	0002000		/* header compare inhibit */

/* THE SC21 ACTUALLY JUST IMPLEMENTS ADVANCE/RETARD... */
#define	UPOF_P400	0020		/*  +400 uinches */
#define	UPOF_M400	0220		/*  -400 uinches */
#define	UPOF_P800	0040		/*  +800 uinches */
#define	UPOF_M800	0240		/*  -800 uinches */
#define	UPOF_P1200	0060		/* +1200 uinches */
#define	UPOF_M1200	0260		/* -1200 uinches */
