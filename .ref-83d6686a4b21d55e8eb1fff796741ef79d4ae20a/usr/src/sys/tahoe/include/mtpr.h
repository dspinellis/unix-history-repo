/*
 *	@(#)mtpr.h	7.1 (Berkeley) %G%
 *	from mtpr.h	4.5	82/11/05
 */

/*
 * TAHOE processor register numbers
 */

#define	SBR	0x0		/* system base register */
#define	SLR	0x1		/* system length register */
#define	P0BR	0x2		/* p0 base register */
#define	P0LR	0x3		/* p0 length register */
#define	P1BR	0x4		/* p1 base register */
#define	P1LR	0x5		/* p1 length register */
#define	P2BR	0x6		/* p2 base register */
#define	P2LR	0x7		/* p2 length register */
#define	IPL	0x8 		/* interrupt priority level */
#define	MME  	0x9		/* memory management enable */
#define	TBIA	0xa		/* translation buffer invalidate all */
#define	TBIS	0xb		/* translation buffer invalidate single */
#define DCK	0xc		/* data cache key */
#define CCK	0xd		/* code cache key */
#define	PCBB	0xe		/* process control block base */
#define	ISP	0xf		/* interrupt stack pointer */
#define	SIRR	0x10		/* software interrupt request */
#define	SISR	0x11		/* software interrupt summary */
#define	SCBB	0x12		/* system control block base */
#define	KSP	0x13		/* kernelack pointer */
#define	USP	0x14		/* user stack pointer */
#define CPMDCB	0x15		/* CP master DCM pointer */
#define PACC	0x17		/* purge all code cache */
#define P1DC	0x18		/* purge one data cache */
#define PADC	0x19		/* purge all data cache */
#define HISR	0x1a		/* hardware interrupt summery register */
#define DCR	0x1b		/* diagnostic control register */
#define PDCS	0x1c		/* purge data cache slot */
