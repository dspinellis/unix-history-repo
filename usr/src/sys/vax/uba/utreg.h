/*	utreg.h	81/11/04	4.1	*/

/*
 * System Industries Model 9700 Tape Drive
 *   emulates TU45 on the UNIBUS
 */

struct	utdevice
{
	u_short	utcs1;		/* control status register 1 */
	short	utwc;		/* word count register */
	u_short	utba;		/* low 16-bits of bus address */
	u_short	utfc;		/* frame counter */
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
#define	UT_GO		01		/* go bit */
/* function codes reside in bits 5-1 */
#define	UT_NOP		(0)		/* no operation */
#define	UT_REWOFFL	(01<<1)		/* rewind offline */
#define UT_LOOP		(02<<1)		/* loop read/write */
#define	UT_REW		(03<<1)		/* rewind */
#define	UT_CLEAR	(04<<1)		/* drive clear */
#define	UT_SENSE	(05<<1)		/* drive sense */
#define	UT_PRESET	(010<<1)	/* read in preset */
#define	UT_DIAGN	(011<<1)	/* diagnostic mode set */
#define	UT_ERASE	(012<<1)	/* erase */
#define	UT_WEOF		(013<<1)	/* write tape mark */
#define	UT_SFORW	(014<<1)	/* forward space block */
#define	UT_SREV		(015<<1)	/* reverse space block */
#define	UT_SFORWF	(016<<1)	/* forward space file */
#define	UT_SREVF	(017<<1)	/* reverse space file */
#define	UT_WCHFORW	(024<<1)	/* write check forward */
#define	UT_WCHREV	(027<<1)	/* write check reverse */
#define	UT_WCOM		(030<<1)	/* write forward */
#define	UT_RCOM		(034<<1)	/* read forward */
#define	UT_RREV		(037<<1)	/* read reverse */
/* the remainder are control and status bits */
#define	UT_IE		0000100		/* interrupt-enable */
#define	UT_RDY		0000200		/* controller ready */
#define	UT_EADDR	0001400		/* extended address bits */
/* bit 10 unused */
#define	UT_DVA		0004000		/* drive available */
/* bit 12 unused */
/* bit 13 - massbus control parity error not emulated */
#define	UT_TRE		0040000		/* transfer error */
#define	UT_SC		0100000		/* special condition */

#define	UT_BITS \
"\10\20SC\17TRE\14DVA\10RDY\7IE\1GO"

/*
 * utcs2 --
 *   error flags and unit select
 */
#define	UTCS2_DLT	0100000		/* data late */
#define	UTCS2_WCE	0040000		/* write check error */
#define	UTCS2_PE	0020000		/* parity error */
#define	UTCS2_NED	0010000		/* non existent drive */
#define	UTCS2_NEM	0004000		/* non existant memory */
#define	UTCS2_PGE	0002000		/* program error */
#define	UTCS2_MXF	0001000		/* missed transfer */
#define	UTCS2_RPE	0000400		/* rom parity error */
#define	UTCS2_OR	0000200		/* output ready (not emulated) */
#define	UTCS2_IR	0000100		/* input ready (not emulated) */
#define	UTCS2_CLR	0000040		/* controller clear */
#define	UTCS2_PAT	0000020		/* parity test */
#define	UTCS2_BAI	0000010		/* UNIBUS address increment inhibit */
/* bits 2-0 unit select */

#define UTCS2_BITS \
"\10\20DLT\17WCE\16PE\15NED\14\NEM\13\PGE\12\MXF\11RPE\10OR\7IR\6CLR\5PAT\4\BAI"

/*
 * utds --
 *   drive status register
 */
#define	UTDS_ATA	0100000		/* attention active */
#define	UTDS_ERR	0040000		/* composite error */
#define	UTDS_PIP	0020000		/* positioning in progress */
#define	UTDS_MOL	0010000		/* medium on line */
#define	UTDS_WRL	0004000		/* write lock */
#define	UTDS_EOT	0002000		/* end of tape */
#define	UTDS_GCR	0001000		/* GCR status */
#define	UTDS_DPR	0000400		/* drive present (always 1) */
#define	UTDS_DRY	0000200		/* drive ready */
#define	UTDS_SSC	0000100		/* slave status change */
#define	UTDS_PES	0000040		/* phase encode status */
#define	UTDS_SDWN	0000020		/* slowing down */
#define	UTDS_IDB	0000010		/* identification burst */
#define	UTDS_TM		0000004		/* tape mark */
#define	UTDS_BOT	0000002		/* beginning of tape */
#define	UTDS_SLA	0000001		/* slave attention */

#define	UTDS_BITS \
"\10\20ATA\17ERR\16PIP\15MOL\14WRL\13EOT\12GCR\11DPR\10DRY\
\7SSC\6PES\5SDWN\4IDB\3TM\2BOT\1SLA"

/*
 * uter --
 *   general error register
 */
#define	UTER_COR	0100000		/* correctible data error */
#define	UTER_UNS	0040000		/* unsafe */
#define	UTER_OPI	0020000		/* operation incomplete */
#define	UTER_DTE	0010000		/* drive timing error */
#define	UTER_NEF	0004000		/* non executable function */
#define	UTER_CS		0002000		/* correctable skew */
#define	UTER_FCE	0001000		/* frame count error */
#define	UTER_NSG	0000400		/* non standard gap */
#define	UTER_PEF	0000200		/* PE format error */
#define	UTER_INC	0000100		/* incorrectable data */
#define	UTER_DPAR	0000040		/* data bus parity error */
#define	UTER_FMT	0000020		/* format error */
#define	UTER_RPE	0000010		/* read data parity error */
#define	UTER_RMR	0000004		/* register modification refused */
#define	UTER_ILR	0000002		/* illegal register (always 0) */
#define	UTER_ILF	0000001		/* illegal function */

/* those errors we consider "hard" errors */
#define	UTER_HARD	(UTER_UNS|UTER_DTE|UTER_NEF|UTER_NSG|UTER_PEF|UTER_INC|\
			 UTER_DPAR|UTER_FMT|UTER_RPE|UTER_RMR|UTER_ILF)

#define	UTER_BITS \
"\10\20COR\17UNS\16DOPI\15DTE\14NEF\13CS\12FCE\11NSG\10PEF\
\7INC\6DPAR\5FMT\4RPE\3RMR\2ILR\1ILF"

/*
 * uttc --
 *   tape control register
 */
#define	UTTC_ACCL	0100000		/* acceleration */
#define	UTTC_FCS	0040000		/* frame count status */
#define	UTTC_TCW	0020000		/* tape control write */
#define	UTTC_EAODTE	0010000		/* enable aborts on data transfer
					   errors (not emulated) */
/* bit 11 not used */
#define	UTTC_DEN	0003400		/* density select (see below) */
#define	UTTC_FMT	0000360		/* format select (see below) */
#define	UTTC_EVPAR	0000010		/* even parity */
/* bits 0-2 are slave select */

/* the bits to stuff in UTTC_DEN */
#define	UT_NRZI		0000000		/* 800 bpi code */
#define	UT_PE		0002000		/* 1600 bpi code */
#define	UT_GCR		0002400		/* 6250 bpi code */

/* tape formats - only PDP-11 standard is supported */
#define	PDP11FMT	0000300		/* PDP-11 standard */

#define	b_repcnt  b_bcount
#define	b_command b_resid
