/*	tmreg.h	4.3	81/02/21	*/

/*
 * TM11 controller registers
 */
struct device {
	u_short	tmer;		/* error register, per drive */
	u_short	tmcs;		/* control-status register */
	short	tmbc;		/* byte/frame count */
	u_short tmba;		/* address */
	short	tmdb;		/* data buffer */
	short	tmrd;		/* read lines */
	short	tmmr;		/* maintenance register */
};

#define	b_repcnt  b_bcount
#define	b_command b_resid

/* bits in tmcs */
#define	TM_GO		0000001
#define	TM_OFFL		0000000		/* offline */
#define	TM_RCOM		0000002		/* read */
#define	TM_WCOM		0000004		/* write */
#define	TM_WEOF		0000006		/* write-eof */
#define	TM_SFORW	0000010		/* space forward */
#define	TM_SREV		0000012		/* space backwards */
#define	TM_WIRG		0000014		/* write with xtra interrecord gap */
#define	TM_REW		0000016		/* rewind */
#define	TM_SENSE	TM_IE		/* sense (internal to driver) */
/* TM_SNS is a pseudo-op used to get tape status */
#define	TM_IE		0000100		/* interrupt enable */
#define	TM_CUR		0000200		/* control unit is ready */
#define	TM_DCLR		0010000		/* drive clear */
#define	TM_D800		0060000		/* select 800 bpi density */
#define	TM_ERR		0100000		/* drive error summary */

/* bits in tmer */
#define	TM_TUR		0000001		/* tape unit ready */
#define	TM_RWS		0000002		/* tape unit rewinding */
#define	TM_WRL		0000004		/* tape unit write protected */
#define	TM_SDWN		0000010		/* gap settling down */
#define	TM_BOT		0000040		/* at beginning of tape */
#define	TM_SELR		0000100		/* tape unit properly selected */
#define	TM_NXM		0000200		/* non-existant memory */
#define	TM_BTE		0000400		/* bad tape error */
#define	TM_RLE		0001000		/* record length error */
#define	TM_EOT		0002000		/* at end of tape */
#define	TM_BGL		0004000		/* bus grant late */
#define	TM_PAE		0010000		/* parity error */
#define	TM_CRE		0020000		/* cyclic redundancy error */
#define	TM_EOF		0040000		/* end of file */
#define	TM_ILC		0100000		/* illegal command */

#define	TM_HARD		(TM_ILC|TM_EOT)
#define	TM_SOFT		(TM_CRE|TM_PAE|TM_BGL|TM_RLE|TM_BTE|TM_NXM)
