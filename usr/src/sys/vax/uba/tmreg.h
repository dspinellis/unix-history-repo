struct device {
	u_short	tmer;
	u_short	tmcs;
	short	tmbc;
	u_short tmba;
	short	tmdb;
	short	tmrd;
};

#define	b_repcnt  b_bcount
#define	b_command b_resid

/* bits in tmcs */
#define	GO	01
#define	OFFL	0
#define	RCOM	02
#define	WCOM	04
#define	WEOF	06
#define	SFORW	010
#define	SREV	012
#define	WIRG	014
#define	REW	016
#define	IENABLE	0100
#define	CUR	0200
#define	NOP	IENABLE
#define	DCLR	010000
#define	D800	060000
#define	ERROR	0100000

/* bits in tmer */
#define	TUR	1
#define	RWS	02
#define	WRL	04
#define	SDWN	010
#define	BOT	040
#define	SELR	0100
#define	NXM	0200
#define	TMBTE	0400
#define	RLE	01000
#define	EOT	02000
#define	BGL	04000
#define	PAE	010000
#define	CRE	020000
#define	EOF	040000
#define	ILC	0100000

#define	HARD    (ILC|EOT)
#define	SOFT	(CRE|PAE|BGL|RLE|TMBTE|NXM)

