/*	hpreg.h	4.10	82/02/08	*/

struct hpdevice
{
	int	hpcs1;		/* control and status register 1 */
	int	hpds;		/* drive status */
	int	hper1;		/* error register 1 */
	int	hpmr;		/* maintenance */ 
	int	hpas;		/* attention summary */
	int	hpda;		/* desired address register */
	int	hpdt;		/* drive type */
	int	hpla;		/* look ahead */
	int	hpsn;		/* serial number */
	int	hpof;		/* offset register */
	int	hpdc;		/* desired cylinder address register */
	int	hpcc;		/* current cylinder */
/* on an rp drive, mr2 is called er2 and er2 is called er3 */
/* we use rm terminology here */
	int	hpmr2;		/* maintenance register 2 */
	int	hper2;		/* error register 2 */
	int	hpec1;		/* burst error bit position */
	int	hpec2;		/* burst error bit pattern */
};

/* hpcs1 */
#define	HP_SC	0100000		/* special condition */
#define	HP_TRE	0040000		/* transfer error */
#define	HP_DVA	0004000		/* drive available */
#define	HP_RDY	0000200		/* controller ready */
#define	HP_IE	0000100		/* interrupt enable */
/* bits 5-1 are the command */
#define	HP_GO	0000001

/* commands */
#define	HP_NOP		000		/* no operation */
#define	HP_UNLOAD	002		/* offline drive */
#define	HP_SEEK		004		/* seek */
#define	HP_RECAL	006		/* recalibrate */
#define	HP_DCLR		010		/* drive clear */
#define	HP_RELEASE	012		/* release */
#define	HP_OFFSET	014		/* offset */
#define	HP_RTC		016		/* return to centerline */
#define	HP_PRESET	020		/* read-in preset */
#define	HP_PACK		022		/* pack acknowledge */
#define	HP_SEARCH	030		/* search */
#define	HP_DIAGNOSE	034		/* diagnose drive */
#define	HP_WCDATA	050		/* write check data */
#define	HP_WCHDR	052		/* write check header and data */
#define	HP_WCOM		060		/* write data */
#define	HP_WHDR		062		/* write header */
#define	HP_WTRACKD	064		/* write track descriptor */
#define	HP_RCOM		070		/* read data */
#define	HP_RHDR		072		/* read header and data */
#define	HP_RTRACKD	074		/* read track descriptor */
	
/* hpds */
#define	HPDS_ATA	0100000		/* attention active */
#define	HPDS_ERR	0040000		/* composite drive error */
#define	HPDS_PIP	0020000		/* positioning in progress */
#define	HPDS_MOL	0010000		/* medium on line */
#define	HPDS_WRL	0004000		/* write locked */
#define	HPDS_LST	0002000		/* last sector transferred */
#define	HPDS_PGM	0001000		/* programmable */
#define	HPDS_DPR	0000400		/* drive present */
#define	HPDS_DRY	0000200		/* drive ready */
#define	HPDS_VV		0000100		/* volume valid */
/* bits 1-5 are spare */
#define	HPDS_OM		0000001		/* offset mode */

#define	HPDS_DREADY	(HPDS_DPR|HPDS_DRY|HPDS_MOL|HPDS_VV)
#define	HPDS_BITS \
"\10\20ATA\17ERR\16PIP\15MOL\14WRL\13LST\12PGM\11DPR\10DRY\7VV\1OM"

/* hper1 */
#define	HPER1_DCK	0100000		/* data check */
#define	HPER1_UNS	0040000		/* drive unsafe */
#define	HPER1_OPI	0020000		/* operation incomplete */
#define	HPER1_DTE	0010000		/* drive timing error */
#define	HPER1_WLE	0004000		/* write lock error */
#define	HPER1_IAE	0002000		/* invalid address error */
#define	HPER1_AOE	0001000		/* address overflow error */
#define	HPER1_HCRC	0000400		/* header crc error */
#define	HPER1_HCE	0000200		/* header compare error */
#define	HPER1_ECH	0000100		/* ecc hard error */
#define HPER1_WCF	0000040		/* write clock fail */
#define	HPER1_FER	0000020		/* format error */
#define	HPER1_PAR	0000010		/* parity error */
#define	HPER1_RMR	0000004		/* register modification refused */
#define	HPER1_ILR	0000002		/* illegal register */
#define	HPER1_ILF	0000001		/* illegal function */

#define	HPER1_BITS \
"\10\20DCK\17UNS\16OPI\15DTE\14WLE\13IAE\12AOE\11HCRC\10HCE\
\7ECH\6WCF\5FER\4PAR\3RMR\2ILR\1ILF"
#define	HPER1_HARD    \
	(HPER1_WLE|HPER1_IAE|HPER1_AOE|\
	 HPER1_FER|HPER1_RMR|HPER1_ILR|HPER1_ILF)

/* hper2 */
#define	HPER2_BSE	0100000		/* bad sector error */
#define	HPER2_SKI	0040000		/* seek incomplete */
#define	HPER2_OPE	0020000		/* operator plug error */
#define	HPER2_IVC	0010000		/* invalid command */
#define	HPER2_LSC	0004000		/* loss of system clock */
#define	HPER2_LBC	0002000		/* loss of bit check */
#define	HPER2_DVC	0000200		/* device check */
#define	HPER2_SSE	0000040		/* skip sector error (rm80) */
#define	HPER2_DPE	0000010		/* data parity error */

#define	HPER2_BITS \
"\10\20BSE\17SKI\16OPE\15IVC\14LSC\13LBC\10DVC\5SSE\4DPE"
#define	HPER2_HARD    (HPER2_OPE)

/* hpof */
#define	HPOF_CMO	0100000		/* command modifier */
#define	HPOF_MTD	0040000		/* move track descriptor */
#define	HPOF_FMT22	0010000		/* 16 bit format */
#define	HPOF_ECI	0004000		/* ecc inhibit */
#define	HPOF_HCI	0002000		/* header compare inhibit */
#define	HPOF_SSEI	0001000		/* skip sector inhibit */

#define	HPOF_P400	020		/*  +400 uinches */
#define	HPOF_M400	0220		/*  -400 uinches */
#define	HPOF_P800	040		/*  +800 uinches */
#define	HPOF_M800	0240		/*  -800 uinches */
#define	HPOF_P1200	060		/* +1200 uinches */
#define	HPOF_M1200	0260		/* -1200 uinches */

/* hpmr */
#define	HPMR_SZ		0174000		/* ML11 system size */
#define	HPMR_ARRTYP	0002000		/* ML11 array type */
#define	HPMR_TRT	0001400		/* ML11 transfer rate */
