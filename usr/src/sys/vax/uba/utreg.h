/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)utreg.h	6.2 (Berkeley) %G%
 */

/*
 * System Industries Model 9700 Tape Drive
 *   emulates TU45 on the UNIBUS
 */

struct utdevice {
	u_short	utcs1;		/* control status register 1 */
	short	utwc;		/* word count register */
	u_short	utba;		/* low 16-bits of bus address */
	short	utfc;		/* frame counter */
	u_short	utcs2;		/* control status register 2 */
	u_short	utds;		/* drive status register */
	u_short	uter;		/* error register */
	u_short	utas;		/* attention status register */
	u_short	utcc;		/* NRZI CRC character for validation */
	u_short	utdb;		/* data buffer reg (not emulated) */
	u_short	utmr;		/* maintenance reg (not emulated) */
	u_short	utdt;		/* drive type register (not emulated) */
	u_short	utsn;		/* serial number reg (not emulated) */
	u_short	uttc;		/* tape control register */
	u_short	utbae;		/* buffer address extension register */
	u_short	utcs3;		/* control and status register 3 */
};

/*
 * utcs1 --
 *   cmds, interrupt enable, extended address bits, and status
 */
#define	UT_GO		0x0001		/* go bit */
/* function codes reside in bits 5-1 */
#define	UT_NOP		0x0000		/* no operation */
#define	UT_REWOFFL	0x0002		/* rewind offline */
#define	UT_LOOP		0x0004		/* loop read/write */
#define	UT_REW		0x0006		/* rewind */
#define	UT_CLEAR	0x0008		/* drive clear */
#define	UT_SENSE	0x000a		/* drive sense */
#define	UT_PRESET	0x0010		/* read in preset */
#define	UT_DIAGN	0x0012		/* diagnostic mode set */
#define	UT_ERASE	0x0014		/* erase */
#define	UT_WEOF		0x0016		/* write tape mark */
#define	UT_SFORW	0x0018		/* forward space block */
#define	UT_SREV		0x001a		/* reverse space block */
#define	UT_SFORWF	0x001c		/* forward space file */
#define	UT_SREVF	0x001e		/* reverse space file */
#define	UT_WCHFORW	0x0028		/* write check forward */
#define	UT_WCHREV	0x002e		/* write check reverse */
#define	UT_WCOM		0x0030		/* write forward */
#define	UT_RCOM		0x0038		/* read forward */
#define	UT_RREV		0x003e		/* read reverse */
/* the remainder are control and status bits */
#define	UT_IE		0x0040		/* interrupt-enable */
#define	UT_RDY		0x0080		/* controller ready */
#define	UT_EADDR	0x0300		/* extended address bits */
/* bit 10 unused */
#define	UT_DVA		0x0800		/* drive available */
/* bit 12 unused */
/* bit 13 - massbus control parity error not emulated */
#define	UT_TRE		0x4000		/* transfer error */
#define	UT_SC		0x8000		/* special condition */

#define	UT_BITS \
"\10\20SC\17TRE\14DVA\10RDY\7IE\1GO"

/*
 * utcs2 --
 *   controller clear, error flags, and unit select
 */
/* bits 0-2 are unit select */
#define	UTCS2_BAI	0x0008		/* UNIBUS address increment inhibit */
#define	UTCS2_PAT	0x0010		/* parity test */
#define	UTCS2_CLR	0x0020		/* controller clear */
#define	UTCS2_IR	0x0040		/* input ready (not emulated) */
#define	UTCS2_OR	0x0080		/* output ready (not emulated) */
#define	UTCS2_RPE	0x0100		/* rom parity error */
#define	UTCS2_MXF	0x0200		/* missed transfer */
#define	UTCS2_NEM	0x0400		/* non existant memory */
#define	UTCS2_PGE	0x0800		/* program error */
#define	UTCS2_NED	0x1000		/* non existent drive */
#define	UTCS2_PE	0x2000		/* parity error */
#define	UTCS2_WCE	0x4000		/* write check error */
#define	UTCS2_DLT	0x8000		/* data late */

#define	UTCS2_BITS \
"\10\20DLT\17WCE\16PE\15NED\14\NEM\13\PGE\12\MXF\11RPE\10OR\7IR\6CLR\5PAT\4\BAI"

/*
 * utds --
 *   beginning of tape, end of tape, error summary bit, plus lots more
 */
#define	UTDS_SLA	0x0001		/* slave attention */
#define	UTDS_BOT	0x0002		/* beginning of tape */
#define	UTDS_TM		0x0004		/* tape mark */
#define	UTDS_IDB	0x0008		/* identification burst */
#define	UTDS_SDWN	0x0010		/* slowing down */
#define	UTDS_PES	0x0020		/* phase encode status */
#define	UTDS_SSC	0x0040		/* slave status change */
#define	UTDS_DRY	0x0080		/* drive ready */
#define	UTDS_DPR	0x0100		/* drive present (always 1) */
#define	UTDS_GCR	0x0200		/* GCR status */
#define	UTDS_EOT	0x0400		/* end of tape */
#define	UTDS_WRL	0x0800		/* write lock */
#define	UTDS_MOL	0x1000		/* medium on line */
#define	UTDS_PIP	0x2000		/* positioning in progress */
#define	UTDS_ERR	0x4000		/* composite error */
#define	UTDS_ATA	0x8000		/* attention active */

#define	UTDS_BITS \
"\10\20ATA\17ERR\16PIP\15MOL\14WRL\13EOT\12GCR\11DPR\10DRY\
\7SSC\6PES\5SDWN\4IDB\3TM\2BOT\1SLA"

/*
 * uter --
 *   detailed breakdown of error summary bit from cs2
 */
#define	UTER_ILF	0x0001		/* illegal function */
#define	UTER_ILR	0x0002		/* illegal register (always 0) */
#define	UTER_RMR	0x0004		/* register modification refused */
#define	UTER_RPE	0x0008		/* read data parity error */
#define	UTER_FMT	0x0010		/* format error */
#define	UTER_DPAR	0x0020		/* data bus parity error */
#define	UTER_INC	0x0040		/* incorrectable data */
#define	UTER_PEF	0x0080		/* PE format error */
#define	UTER_NSG	0x0100		/* non standard gap */
#define	UTER_FCE	0x0200		/* frame count error */
#define	UTER_CS		0x0400		/* correctable skew */
#define	UTER_NEF	0x0800		/* non executable function */
#define	UTER_DTE	0x1000		/* drive timing error */
#define	UTER_OPI	0x2000		/* operation incomplete */
#define	UTER_UNS	0x4000		/* unsafe */
#define	UTER_COR	0x8000		/* correctible data error */

/*
 * These errors we consider "hard"; UTER_OPI and UTER_RPE
 * are considered "soft", at least for the moment.
 */
#define	UTER_HARD	(UTER_UNS|UTER_NEF|UTER_DPAR|UTER_FMT|UTER_RMR|\
			 UTER_ILR|UTER_ILF)

#define	UTER_BITS \
"\10\20COR\17UNS\16OPI\15DTE\14NEF\13CS\12FCE\11NSG\10PEF\
\7INC\6DPAR\5FMT\4RPE\3RMR\2ILR\1ILF"

/*
 * uttc --
 *   tape format and density
 */
/* bits 0-2 are slave select */
#define	UTTC_EVPAR	0x0008		/* even parity */
#define	UTTC_FMT	0x00f0		/* format select (see below) */
#define	UTTC_DEN	0x0700		/* density select (see below) */
/* bit 11 not used */
#define	UTTC_EAODTE	0x1000		/* (not emulated) */
#define	UTTC_TCW	0x2000		/* tape control write */
#define	UTTC_FCS	0x4000		/* frame count status */
#define	UTTC_ACCL	0x8000		/* acceleration */

/* the bits to stuff in UTTC_DEN */
#define	UT_NRZI		0x0000		/* 800 bpi code */
#define	UT_PE		0x0400		/* 1600 bpi code */
#define	UT_GCR		0x0500		/* 6250 bpi code */

/* tape formats - only PDP-11 standard is supported */
#define	PDP11FMT	0x00c0		/* PDP-11 standard */

#define	b_repcnt  b_bcount
#define	b_command b_resid
#define	b_state	  b_active  
