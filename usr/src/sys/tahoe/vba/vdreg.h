/*	vdreg.h	1.1	86/01/05	*/

/*
 * VDDC (Versabus Direct Disk Controller) definitions.
 */

/*
 * DCB Command Codes
 */
#define	RD		0x80		/* Read Data */
#define	FTR		0xc0		/* Full Track Read */
#define	RAS		0x90		/* Read and Scatter */
#define	C		0xa0		/* Compare */
#define	FTC		0xe0		/* Full Track Compare */
#define	RHDE		0x180		/* Read Header, Data & ECC (not used) */
#define	WD		0x00		/* Write Data */
#define	FTW		0x40		/* Full Track Write */
#define	WTC		0x20		/* Write Then Compare */
#define	FTWTC		0x60		/* Full Track Write Then Compare */
#define	GAW		0x10		/* Gather and Write */
#define	WDE		0x100		/* Write Data & ECC (not used) */
#define	FSECT		0x900		/* Format Sector */
#define	GWC		0x30		/* Gather Write & Compare */
#define	VDSTART		0x800		/* Start drives */
#define	VDRELEASE	0xa00		/* Stop drives */
#define	SEEK		0xb00		/* Seek */
#define	INIT		0xc00		/* Initialize VDDC */
#define	DIAG		0xd00		/* Diagnose (self-test) VDDC */
#define	RSTCFG		0xe00		/* Reset/Configure VDDC/DDI/Drive(s) */
#define	VDSTATUS	0xf00		/* VDDC Status */

#define	ABORT		0x80000000	/* ABORT active i/o */

/*
 * Error/status codes.
 */
#define	HCRCERR		0x1		/* Header CRC Error */
#define	HCMPERR		0x2		/* Header Compare Error */
#define	WPTERR		0x4		/* Write Protect Error/Status */
#define	CTLRERR		0x8		/* Controller Error */
#define	DSEEKERR	0x10		/* Disk Seek Error */
#define	UCDATERR	0x20		/* Uncorrectable Data Error */
#define	NOTCYLERR	0x40		/* Not on Cylinder Error */
#define	DRVNRDY		0x80		/* Drive Not Ready Error/Status */
#define	ALTACC		0x100		/* Alternate (track) accessed Status */
#define	SEEKSTRT	0x200		/* Seek Started Status */
#define	INVDADR		0x400		/* Invalid Disk Address Error */
#define	DNEMEM		0x800		/* Non-Existant Memory Error */
#define	PARERR		0x1000		/* Memory Parity Error */
#define	DCOMPERR	0x2000		/* Data Compare Error */
#define	DDIRDY		0x4000		/* DDI Ready Error/Status */
#define	OPABRT		0x8000		/* Operator Abort (Host) Error/Status */
#define	DSERLY		0x10000		/* Data Strobe Early */
#define	DSLATE		0x20000		/* Data Strobe Late */
#define	TOPLUS		0x40000		/* Track Offset Plus */
#define	TOMNUS		0x80000		/* Track Offset Minus */
#define	CPDCRT		0x100000	/* Cntlr Performed Data Correction */
#define	HRDERR		0x200000	/* Hard Error */
#define	SFTERR		0x400000	/* Soft Error (retry succesful) */
#define	ANYERR		0x800000	/* Any Error */
#define INVCMD		0x1000000	/* Programmer error */

/* hard error */
#define	HTYPES \
    (HCRCERR|HCMPERR|WPTERR|CTLRERR|DSEEKERR|UCDATERR|NOTCYLERR|DRVNRDY|\
     INVDADR|DNEMEM|PARERR|DCOMPERR)

#define	ERRS	0x3FFF
/* retryable errors */
#define	CANRETRY \
    (CTLRERR|DSEEKERR|NOTCYLERR|DCOMPERR|UCDATERR|PARERR|DNEMEM|HCRCERR|HCMPERR)

#define	ERRBITS	"\20\1HCRC\2HCMP\3WPT\4CTLR\5DSEEK\6UCDATA\7NOTCYL\10DRVNRDY\
\11ALTACC\12SEEKSTRT\13INVDADR\14DNEMEM\15PAR\16DCOMP\17DDIRDY\20OPABRT\
\21DSERLY\22DSLATE\23TOPLUS\24TOPMNUS\25CPDCRT\26HRDERR\27SFTERR\30ANYERR\
\31INVCMD"

/*
 * DCB status codes.
 */
#define	DCBABT		0x10000000	/* DCB Aborted */
#define	DCBUSC		0x20000000	/* DCB Unsuccesfully Completed */
#define	DCBCMP		0x40000000	/* DCB Complete */
#define	DCBSTR		0x80000000	/* DCB Started */

/*
 * MDCB status codes.
 */
#define	CTLRBSY		0x10000000	/* Cntlr Busy */
#define	INTCCDE		0x60000000	/* Interrupt Cause Code */
#define	DCBINT		0x80000000	/* DCB Interrupt Flag */

/*
 * VDDC interrupt modes.
 */
#define	NOINT	0x0		/* No Interrupt */
#define	INTERR	0x2		/* Interrupt on Error */
#define	INTSUC	0x1		/* Interrupt on Success */
#define	INTDONE	0x3		/* Interrupt on Error or Success */


/*
 * Constrol status definitions.
 */
#define	CS_SCS	0xf		/* Status Change Source (drive number) */
#define	CS_ELC	0x10		/* Error on Last Command */
#define	CS_ICC	0x60		/* Interupt Cause Code */
#define   ICC_NOI  0x00		/* No interupt */
#define   ICC_DUN  0x20		/* No interupt */
#define   ICC_ERR  0x40		/* No interupt */
#define   ICC_SUC  0x60		/* No interupt */
#define	CS_GO	0x80		/* Go bit (controller working) */
#define	CS_BE	0x100		/* Buss Error */
#define	CS_BOK	0x4000		/* Board O.K. */
#define	CS_SFL	0x8000		/* System fail */
#define	CS_LEC	0xff000000	/* Last Error Code */

/* Status word bit assignments */
#define	STA_UR	0x1		/* Unit Ready */
#define	STA_OC	0x2		/* On Cylinder */
#define	STA_SE	0x4		/* Seek Error */
#define	STA_DF	0x8		/* Drive Fault */
#define	STA_WP	0x10		/* Write Protected */
#define	STA_US	0x20		/* Unit Selected */

/* Interupt Control Field bit assignments */
#define	ICF_IPL	0x7		/* Interupt Priority Level */
#define	ICF_IEN	0x8		/* Interupt ENable */
#define	ICF_IV	0xff00		/* Interupt Vector */

/* Transfer Control Format bit assignments */
#define	TCF_AM	0xff		/* Address Modifier */
#define	  AM_SNPDA   0x01	/* Standard Non-Privileged Data Access */
#define	  AM_SASA    0x81	/* Standard Ascending Sequential Access */
#define	  AM_ENPDA   0xf1	/* Extended Non-Privileged Data Access */
#define	  AM_EASA    0xe1	/* Extended Ascending Sequential Access */
#define	TCF_BTE	0x800		/* Block Transfer Enable */

/* Controller Configuration Flags bit assignments */
#define	CCF_STS	0x1		/* Sectors per Track Selectable */
#define	CCF_EAV	0x2		/* Enable Auto Vector */
#define	CCF_ERR	0x4		/* Enable Reset Register */
#define	CCF_XMD	0x60		/* XMD transfer mode (buss size) */
#define	  XMD_8BIT  0x20	/*   Do only 8 bit transfers */
#define	  XMD_16BIT 0x40	/*   Do only 16 bit transfers */
#define	  XMD_32BIT 0x60	/*   Do only 32 bit transfers */
#define	CCF_BSZ	0x300		/* Burst SiZe */
#define	  BSZ_16WRD 0x000	/*   16 word transfer burst */
#define	  BSZ_12WRD 0x100	/*   12 word transfer burst */
#define	  BSZ_8WRD  0x200	/*   8 word transfer burst */
#define	  BSZ_4WRD  0x300	/*   4 word transfer burst */
#define	CCF_ENP	0x1000		/* ENable Parity */
#define	CCF_EPE	0x2000		/* Enable Parity Errors */
#define	CCF_EDE	0x10000		/* Error Detection Enable */
#define	CCF_ECE	0x20000		/* Error Correction Enable */

/*
 * Diagnostic register definitions.
 */
#define	DIA_DC	0x7f		/* Dump count mask */
#define	DIA_DWR	0x80		/* Dump Write / Read flag */
#define	DIA_ARE	0x100		/* Auto Rebuild Enable */
#define	DIA_CEN	0x200		/* Call ENable flag */
#define	DIA_KEY	0xAA550000	/* Reset KEY */

/* Sector Header bit assignments */
#define	VDMF	0x8000		/* Manufacturer Fault 1=good sector */
#define	VDUF	0x4000		/* User Fault 1=good sector */
#define	VDALT	0x2000		/* Alternate Sector 1=alternate */
#define	VDWPT	0x1000		/* Write Protect 1=Read Only Sector */

/* DCB Bit assignments */
#define	INT_IC	0x3		/* Interupt Control */
#define	  IC_NOI  0x0		/*   NO Interupt */
#define	  IC_IOD  0x1		/*   Interupt On Done */
#define	  IC_IOE  0x2		/*   Interupt On Error */
#define	  IC_IOS  0x3		/*   Interupt On Success */
#define	INT_PBA	0x4		/* Proceed before ACK */

/*
 * Perform a reset on the controller.
 */
#define	VDDC_RESET(addr, type) { \
	if (type == SMD_ECTLR) { \
		(addr)->diag_flags = DIA_KEY|DIA_CEN; \
		(addr)->cdr_mdcb_ptr = (fmt_mdcb *)0xffffffff; \
		DELAY(5000000); \
	} else { \
		(addr)->cdr_reset = 0x0; \
		DELAY(1500000); \
	} \
}

/*
 * Abort a controller operation.
 */
#define	VDDC_ABORT(a, type) { \
	if ((type) == SMDCTLR) { \
		movow(a, (ABORT & 0xffff0000) >> 16) ; \
		movow((int)(a)+2, ABORT & 0xffff); \
	} else \
		(a)->cdr_mdcb_ptr = (fmt_mdcb *)ABORT; \
	DELAY(1000000); \
}

/*
 * Start i/o on controller.
 */
#define VDDC_ATTENTION(ctlr, mdcbadr, type) {\
	if (type == SMDCTLR) { \
		movow(ctlr, ((int)mdcbadr & 0xffff0000) >> 16) ; \
		movow((int)(ctlr)+2, (int)mdcbadr & 0xffff); \
	} else \
		(ctlr)->cdr_mdcb_ptr = mdcbadr; \
}

/*
 * Poll controller until operation completes
 * or timeout expires.
 * YECH!!!! THIS SHOULD BE A SUBROUTINE!!!
 */
#define	POLLTILLDONE(c, a, x, t) { \
	vdtimeout = 1000 * (x); \
	uncache(&(a)->operrsta); \
	while ((((a)->operrsta) & (DCBCMP|DCBABT)) == 0) { \
		DELAY(1000); \
		vdtimeout--; \
		uncache(&(a)->operrsta); \
		if (vdtimeout <= 0) { \
			printf("vd%d: controller timeout", c); \
			VDDC_ABORT(c, t); \
			DELAY(30000); \
			break; \
		} \
	} \
	if (vdtimeout > 0) \
		if ((t) == SMD_ECTLR && vdtimeout > 0) { \
			uncache(&(c)->cdr_csr); \
			while((c)->cdr_csr&CS_GO) { \
				DELAY(50); \
				uncache(&(c)->cdr_csr); \
			} \
			DELAY(500); \
		} else \
			DELAY(200); \
	uncache(&(a)->operrsta); \
}

/*
 * A disk address.
 */
typedef struct {
	char	track;			/* all 8 bits */
	char	sector;			/* all 8  bits */
	short	cylinder;		/* low order 12 bits */
} dskadr;

/*
 * Sector formats.
 */
typedef union {
	struct {
		dskadr	hdr_addr;
		short	smd_crc;
	} smd;
	struct {
		dskadr	physical;
		dskadr	logical;
		long	smd_e_crc;
	} smd_e;
} fmt_hdr;

/*
 * DCB trailer formats.
 */
/* read/write trailer */
typedef struct {
	char	*memadr;	/* memory address */
	u_long	wcount;		/* 16 bit word count */
	dskadr	disk;		/* disk address */
} trrw;

/* scatter/gather trailer */
typedef struct {
	trrw	start_addr;
	struct {
		char	*nxt_addr;
		u_long	nxt_len;
	} addr_chain[126];
} trsg;

/* seek trailer format */
typedef struct {
	dskadr	skaddr;
} trseek;

/* format trailer */
typedef struct {
	char	*addr;		/* data buffer to be filled on sector*/
	long	nsectors;	/* # of sectors to be formatted */
	dskadr	disk;		/* disk physical address info */
	dskadr  hdr;		/* header address info */
} trfmt;

/* reset/configure trailer */
typedef struct {
	long	ncyl;		/* # cylinders */
	long	nsurfaces;	/* # surfaces */
	long	nsectors;	/* # sectors */
	long	slip_sec;	/* # of slip sectors */
} treset;

/*
 * DCB layout.
 */
typedef struct fmtdcb {
	struct	fmtdcb *nxtdcb;	/* next dcb */
	short	intflg;		/* interrupt settings and flags */
	short	opcode;		/* DCB command code etc... */
	long	operrsta;	/* error & status info */
	short	fill;		/* not used */
	char	devselect;	/* drive selection */
	char	trailcnt;	/* trailer Word Count */
	long	err_memadr;	/* error memory address */
	char	err_code;	/* error codes for SMD/E */
	char	fill2;		/* not used */
	short	err_wcount;	/* error word count */
	char	err_trk;	/* error track/sector */
	char	err_sec;	/* error track/sector */
	short	err_cyl;	/* error cylinder adr */
	union {
		trseek	sktrail;	/* seek command trailer */
#ifdef notdef
		trsg	sgtrail;	/* scatter/gather trailer */
#endif
		trrw	rwtrail;	/* read/write trailer */
		trfmt	fmtrail;	/* format trailer */
		treset	rstrail;	/* reset/configure trailer */
	} trail;
} fmt_dcb;

/*
 * MDCB layout.
 */
typedef struct {
	fmt_dcb	*firstdcb;	/* first dcb in chain */
	fmt_dcb	*procdcb;	/* dcb being processed */
	fmt_dcb	*intdcb;	/* dcb causing interrupt */
	long	vddcstat;	/* VDDC status */
} fmt_mdcb;

/*
 * Control-status communications block.
 */
typedef struct {
	fmt_mdcb *cdr_mdcb_ptr;	/* controller's mdcb */
	u_long	cdr_reset;	/* controller reset register */
	u_long	cdr_csr;	/* control/status register */
	long	cdr_reserved;	/* reserved */
	u_short	cdr_status[16];	/* per-drive status register */
	u_short	stat_chng;	/* status change interupt register */
	u_short	done_icf;	/* interupt-complete register */
	u_short	error_icf;	/* error-interupt register */
	u_short	success_icf;	/* success-interupt register */
	u_short	mdcb_tcf;	/* mdcb transfer control register */
	u_short	dcb_tcf;	/* dcb transfer control register */
	u_short	trail_tcf;	/* trail transfer control register */
	u_short	data_tcf;	/* data transfer control register */
	u_long	cdr_ccf;	/* controller configuration flags */
	u_long	sec_size;	/* drive sector size */
	u_long	diag_flags;	/* diagnostic flag register */
	u_long	diag_dump;	/* pointer for diagnostic addresses */
} cdr;

/* controller types */
#define	UNKNOWN		-1
#define	SMDCTLR		1	/* smd interface */
#define	SMD_ECTLR	2	/* extended-smd interface */

/* drive types */
#define	XSD	0
#define	FUJ	1 		/* fujitsu */
#define	XFD	2		/* CDC 340Mb Winchester */
#define	SMD	3		/* CDC 9766 or equivalent */
#define	FSD	4

/*
 * Drive logical partitions.
 */
typedef struct {
	long	par_start;	/* starting sector # */
	long	par_len;	/* size in sectors */
} par_tab;

typedef struct {
	int	secsize;		/* bytes/sector */
	int	nsec;			/* sectors/track */
	int	ntrak;			/* tracks/cylinder */
	int	ncyl;			/* # cylinders */
	int	nslip;			/* # slip sectors */
	int	rpm;			/* revolutions/minute */
	int	nbits;			/* bits/track */
	char	*type_name;		/* drive name */
	long	fmt_pat[16];		/* patterns to be used for formatting */
	par_tab	partition[8];		/* partition tables */
} fs_tab;

/* physical information for known disk drives.  */
#ifdef VDGENDATA
long	vddcaddr[] = { 0xf2000, 0xf2100, 0xf2200, 0xf2300 };
long	vdtimeout = 0;

fs_tab	vdst[] = {
	{512, 48, 24, 711, 0, 3600, 0,	"xsd",	/* 515 Mb FSD */
		{ 0x0264c993, 0x04c99326, 0x0993264c, 0x13264c98,
		  0x264c9930, 0x4c993260, 0x993264c0, 0x3264c980,
		  0x64c99300, 0xc9932600, 0x93264c00, 0x264c9800,
		  0x4c993000, 0x99326000, 0x3264c000, 0x54c98000},
		{{0,	 30528},	/* xsd0a cyl   0 - 52 */
		{30528,	 30528},	/* xsd0b cyl  53 - 105 */
		{61056,	 345600}, 	/* xsd0c cyl 106 - 705 */
		{0,	 61056}, 	/* xsd0d cyl 709 - 710 (a & b) */
		{0,	 406656},	/* xsd0e cyl   0 - 705 */
		{30528,	 376128}, 	/* xsd0f cyl  53 - 705 (b & c) */
		{61056,	 172800},	/* xsd0g cyl 106 - 405 (1/2 of c) */
		{233856, 172800}}	/* xsd0h cyl 406 - 705 (1/2 of c) */
	},
	{512, 64, 10, 823, 0, 3600, 0,	"fuj",	/* 360 Mb Fujitsu */
		{ 0x0264c993, 0x04c99326, 0x0993264c, 0x13264c98,
		  0x264c9930, 0x4c993260, 0x993264c0, 0x3264c980,
		  0x64c99300, 0xc9932600, 0x93264c00, 0x264c9800,
		  0x4c993000, 0x99326000, 0x3264c000, 0x54c98000},
		{{0,	 19200},	/* fuj0a cyl   0 - 59 */
		{19200,	 24000},	/* fuj0b cyl  60 - 134 */
		{43200,	 218560}, 	/* fuj0c cyl 135 - 817 */
		{0,	 43200}, 	/* fuj0d cyl 821 - 822 (a & b) */
		{0,	 261760},	/* fuj0e cyl   0 - 817 */
		{19200,	 242560}, 	/* fuj0f cyl   0 - 134 (b & c) */
		{43200,  109440},	/* fuj0g cyl 135 - 476 (1/2 of c) */
		{152640, 109120}}	/* fug0h cyl 477 - 817 (1/2 of c) */
	},
	{512, 32, 24, 711, 0, 3600, 0,	"xfd",	/* 340 Mb FSD */
		{ 0x0d9b366c, 0x1b366cd8, 0x366cd9b0, 0x6cd9b360,
		  0xd9b366c0, 0xb366cd80, 0x66cd9b00, 0xcd9b3600,
		  0x9b366300, 0x366cd800, 0x6cd9b000, 0xd9b36000,
		  0xb366c000, 0x66cd8000, 0xcd9b0000, 0x9b360000},
#ifdef MICKEY
		{{ 0,	 20352 },	/* xfd0a cyl   0-52 */
		{ 20352, 20352 },	/* xfd0b cyl  53-105 */
		{ 40704, 230400 },	/* xfd0c cyl 106-705 */
		{ 271104,1920 },	/* xfd0d cyl 706-710 */
		{ 0,	 271104 },	/* xfd0e cyl   0-705 */
		{ 0,	 273024 }},	/* xfd0f cyl   0-710 */
#else
		{{ 0,	 20352 },	/* xfd0a cyl   0 - 52 */
		{ 20352, 20352 },	/* xfd0b cyl  53 - 105 */
		{ 40704, 230400 },	/* xfd0c cyl 106 - 705 */
		{ 0,	 40704 },	/* xfd0d cyl 709 - 710 (a & b) */
		{ 0,	 271104 },	/* xfd0e cyl   0 - 705 */
		{ 20352, 250752 },	/* xfd0f cyl  53 - 705 (b & c) */
		{ 40704, 115200 },	/* xfd0g cyl 106 - 405 (1/2 of c) */
		{ 155904,115200 }}	/* xfd0h cyl 406 - 705 (1/2 of c) */
#endif
	},
	{512, 32, 19, 823, 0, 3600, 0,	"smd",	/* 300 Mb SMD */
		{ 0x0d9b366c, 0x1b366cd8, 0x366cd9b0, 0x6cd9b360,
		  0xd9b366c0, 0xb366cd80, 0x66cd9b00, 0xcd9b3600,
		  0x9b366300, 0x366cd800, 0x6cd9b000, 0xd9b36000,
		  0xb366c000, 0x66cd8000, 0xcd9b0000, 0x9b360000},
		{{ 0,	 20064},	/* smd0a cyl   0-65 */
		{ 20064, 13680},	/* smd0b cyl  66-110 */
		{ 33744, 214928},	/* smd0c cyl 111-817 */
		{ 248672,1520 },	/* smd0d cyl 818-822 */
		{ 0,	 248672 },	/* smd0e cyl   0-817 */
		{ 0,	 250192 }},	/* smd0f cyl   0-822 */
	},
	{512, 32, 10, 823, 0, 3600, 0,	"fsd",	/* 160 Mb FSD */
		{ 0x0d9b366c, 0x1b366cd8, 0x366cd9b0, 0x6cd9b360,
		  0xd9b366c0, 0xb366cd80, 0x66cd9b00, 0xcd9b3600,
		  0x9b366300, 0x366cd800, 0x6cd9b000, 0xd9b36000,
		  0xb366c000, 0x66cd8000, 0xcd9b0000, 0x9b360000},
		{{0,	 9600},		/* fsd0a cyl   0 -  59 */
		{9600,	 12000},	/* fsd0b cyl  60 - 134 */
		{21600,	 109280},	/* fsd0c cyl 135 - 817 */
		{0,	 21600},	/* fsd0d cyl   0 - 134 (a & b) */
		{0,	 130880},	/* fsd0e cyl   0 - 817 */
		{9600,	 121280},	/* fsd0f cyl  60 - 817 (b & c) */
		{21600,  54240},	/* fsd0g cyl 135 - 473 (1/2 of c) */
		{75840,  55040}}	/* fsd0h cyl 474 - 817 (1/2 of c) */
	}
};

int	nvddrv = (sizeof (vdst) / sizeof (fs_tab));

#else
#ifdef STANDALONE
extern long	vddcaddr[];
extern long	vdtimeout;
extern fs_tab	vdst[];
extern int	nvddrv;
#endif
#endif
