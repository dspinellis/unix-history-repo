/*	idcreg.h	4.1	82/05/26	*/

#define	NRB02SECT	40	/* RB02 sectors/track */
#define	NRB02TRK	2	/* RB02 tracks/cylinder */
#define	NRB02CYL	512	/* RB02 cylinders/disk */
#define	NRB80SECT	31	/* RB80 sectors/track */
#define	NRB80TRK	14	/* RB80 tracks/cylinder */
#define	NRB80CYL	559	/* RB80 cylinders/disk */

struct idcdevice
{
	int	idccsr;		/* control status register */
	int	idcbar;		/* bus address register */
	int	idcbcr;		/* byte count register */
	int	idcdar;		/* disk address register */
	int	idcmpr;		/* multi-purpose register */
	int	idceccpos;	/* ecc position register */
	int	idceccpat;	/* ecc pattern register */
	int	idcreset;	/* master reset register */
};

/* idccsr */
#define	IDC_TOI		0x10000000	/* time out inhibit */
#define	IDC_ASSI	0x08000000	/* automatic skip sector inhibit */
#define	IDC_R80		0x04000000	/* selected disk is R80 */
#define	IDC_MTN		0x02000000	/* maintenance */
#define	IDC_IR		0x01000000	/* interrupt request */
#define	IDC_SSE		0x00800000	/* R80 skip sector error */
#define	IDC_SSEI	0x00400000	/* R80 skip sector error inhibit */
#define	IDC_ECS		0x00300000	/* R80 ecc status */
#define	IDC_ECS_NONE	0x00000000	/*   no data error */
#define	IDC_ECS_HARD	0x00200000	/*   hard ecc error */
#define	IDC_ECS_SOFT	0x00300000	/*   soft ecc error */
#define	IDC_ATTN	0x000f0000	/* attention bits */
#define	IDC_ERR		0x00008000	/* composite error */
#define	IDC_DE		0x00004000	/* drive error */
#define	IDC_NXM		0x00002000	/* non-existant memory */
#define	IDC_DLT		0x00001000	/* data late */
#define	IDC_HNF		IDC_DLT		/* header not found */
#define	IDC_DCK		0x00000800	/* data check */
#define	IDC_OPI		0x00000400	/* operation incomplete */
#define	IDC_DS		0x00000300	/* drive select bits */
#define	IDC_CRDY	0x00000080	/* controller ready */
#define	IDC_IE		0x00000040	/* interrupt enable */
#define	IDC_FUNC	0x0000000e	/* function code */
#define	IDC_DRDY	0x00000001	/* drive ready */

#define	IDC_HARD	(IDC_NXM|IDC_DE)

#define	IDCCSR_BITS \
"\20\35TOI\34ASSI\33R80\32MTN\31IR\30SSE\27SSEI\26ECS1\25ECS0\24ATN3\
\23ATN2\22ATN1\21ATN0\20ERR\17DE\16NXM\15DLT\14DCK\13OPI\12DS1\11DS0\
\10CRDY\7IE\4F2\3F1\2F0\1DRDY"

/* function codes */
#define	IDC_NOP		000		/* no operation */
#define	IDC_WCHK	002		/* write check data */
#define	IDC_GETSTAT	004		/* get status */
#define	IDC_SEEK	006		/* seek */
#define	IDC_RHDR	010		/* read header */
#define	IDC_WRITE	012		/* write data */
#define	IDC_READ	014		/* read data */
#define	IDC_RNOHCHK	016		/* read data w/o header check */

/* idcmpr for RL02 get status command */
#define	IDCGS_RST	010		/* reset */
#define	IDCGS_GS	002		/* get status, must be 1 */
#define	IDCGS_M		001		/* mark, must be 1 */
#define	IDCGS_GETSTAT	(IDCGS_RST|IDCGS_GS|IDCGS_M)

/* RL02 status word */
#define	IDCDS_WDE	0100000		/* write data error */
#define	IDCDS_HCE	0040000		/* head current error */
#define	IDCDS_WL	0020000		/* write lock */
#define	IDCDS_SKTO	0010000		/* seek timeout */
#define	IDCDS_SPD	0004000		/* spindle error */
#define	IDCDS_WGE	0002000		/* write gate error */
#define	IDCDS_VC	0001000		/* volume check */
#define	IDCDS_DSE	0000400		/* drive select error */
#define	IDCDS_HS	0000100		/* head select */
#define	IDCDS_CO	0000040		/* cover open */
#define	IDCDS_HO	0000020		/* heads out */
#define	IDCDS_BH	0000010		/* brush home */
#define	IDCDS_STATE	0000007		/* drive state */

#define	IDCRB02DS_BITS \
"\10\20WDE\17HCE\16WL\15SKTO\14SPD\13WBE\12VC\11DSE\
\7HS\6CO\5HO\4BH\3STC\2STB\1STA"

/* R80 status word */
#define	IDCDS_WTP	0020000		/* write protect */
#define	IDCDS_DRDY	0010000		/* driver ready */
#define	IDCDS_ONCY	0004000		/* on cylinder */
#define	IDCDS_SKE	0002000		/* seek error */
#define	IDCDS_PLGV	0001000		/* plug valid */
#define	IDCDS_FLT	0000400		/* fault */

#define	IDCRB80DS_BITS \
"\10\16WTP\15DRDY\14ONCY\13SKE\12PLGV\11FLT\5SEC4\4SEC3\3SEC2\2SEC1\1SEC0"
