/*	hpreg.h	4.4	81/02/25	*/

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
	int	hper2;		/* error register 2 */
	int	hper3;		/* error register 3 */
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
#define	HP_ATA		0100000		/* attention active */
#define	HP_ERR		0040000		/* composite drive error */
#define	HP_PIP		0020000		/* positioning in progress */
#define	HP_MOL		0010000		/* medium on line */
#define	HP_WRL		0004000		/* write locked */
#define	HP_LST		0002000		/* last sector transferred */
#define	HP_PGM		0001000		/* programmable */
#define	HP_DPR		0000400		/* drive present */
#define	HP_DRY		0000200		/* drive ready */
#define	HP_VV		0000100		/* volume valid */
/* bits 1-5 are spare */
#define	HP_OM		0000001		/* offset mode */

#define	HP_DREADY	(HP_DPR|HP_DRY|HP_MOL|HP_VV)
#define	HPDS_BITS \
"\10\20ATA\17ERR\16PIP\15MOL\14WRL\13LST\12PGM\11DPR\10DRY\7VV\1OM"

/* hper1 */
#define	HP_DCK		0100000		/* data check */
#define	HP_UNS		0040000		/* drive unsafe */
#define	HP_OPI		0020000		/* operation incomplete */
#define	HP_DTE		0010000		/* drive timing error */
#define	HP_WLE		0004000		/* write lock error */
#define	HP_IAE		0002000		/* invalid address error */
#define	HP_AOE		0001000		/* address overflow error */
#define	HP_HCRC		0000400		/* header crc error */
#define	HP_HCE		0000200		/* header compare error */
#define	HP_ECH		0000100		/* ecc hard error */
#define HP_WCF		0000040		/* write clock fail */
#define	HP_FER		0000020		/* format error */
#define	HP_PAR		0000010		/* parity error */
#define	HP_RMR		0000004		/* register modification refused */
#define	HP_ILR		0000002		/* illegal register */
#define	HP_ILF		0000001		/* illegal function */

#define	HPER1_BITS \
"\10\20DCK\17UNS\16OPI\15DTE\14WLE\13IAE\12AOE\11HCRC\10HCE\
\7ECH\6WCF\5FER\4PAR\3RMR\2ILR\1ILF"
/* THIS NEEDS TO BE DOUBLE CHECKED... */
#define	HPER1_HARD    (HP_UNS|HP_WLE|HP_IAE|HP_AOE|HP_FER|HP_RMR|HP_ILR|HP_ILF)

/* hper2 */
#define	HP_BSE		0100000		/* bad sector error */
#define	HP_SKI		0040000		/* seek incomplete */
#define	HP_OPE		0020000		/* operator plug error */
#define	HP_IVC		0010000		/* invalid command */
#define	HP_LSC		0004000		/* loss of system clock */
#define	HP_LBC		0002000		/* loss of bit check */
#define	HP_DVC		0000200		/* device check */
#define	HP_DPE		0000010		/* data parity error */

#define	HPER2_BITS \
"\10\20BSE\17SKI\16OPE\15IVC\14LSC\13LBC\10DVC\4DPE"
#define	HPER2_HARD    (HP_BSE|HP_OPE)

/* hpof */
#define	HP_CMO		0100000		/* command modifier */
#define	HP_MTD		0040000		/* move track descriptor */
#define	HP_FMT22	0010000		/* 16 bit format */
#define	HP_ECI		0004000		/* ecc inhibit */
#define	HP_HCI		0002000		/* header compare inhibit */
#define	HP_SSEI		0001000		/* skip sector inhibit */

#define	HP_P400		020		/*  +400 uinches */
#define	HP_M400		0220		/*  -400 uinches */
#define	HP_P800		040		/*  +800 uinches */
#define	HP_M800		0240		/*  -800 uinches */
#define	HP_P1200	060		/* +1200 uinches */
#define	HP_M1200	0260		/* -1200 uinches */
