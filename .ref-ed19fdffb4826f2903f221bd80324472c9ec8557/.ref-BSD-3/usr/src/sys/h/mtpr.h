/*	mtpr.h	2.1	1/5/80 */

/*
 * VAX processor register numbers
 */

#define	KSP	0		/* kernel stack pointer */
#define	ESP	1		/* exec stack pointer */
#define	SSP	2		/* supervisor stack pointer */
#define	USP	3		/* user stack pointer */
#define	ISP	4		/* interrupt stack pointer */
#define	P0BR	8		/* p0 base register */
#define	P0LR	9		/* p0 length register */
#define	P1BR	10		/* p1 base register */
#define	P1LR	11		/* p1 length register */
#define	SBR	12		/* system segment base register */
#define	SLR	13		/* system segment length register */
#define	PCBB	16		/* process control block base */
#define	SCBB	17		/* system control block base */
#define	IPL	18		/* interrupt priority level */
#define	ASTLVL	19		/* async. system trap level */
#define	SIRR	20		/* software interrupt request */
#define	SISR	21		/* software interrupt summary */
#define	ICCS	24		/* interval clock control */
#define	NICR	25		/* next interval count */
#define	ICR	26		/* interval count */
#define	TODR	27		/* time of year (day) */
#define	RXCS	32		/* console receiver control and status */
#define	RXDB	33		/* console receiver data buffer */
#define	TXCS	34		/* console transmitter control and status */
#define	TXDB	35		/* console transmitter data buffer */
#define	MAPEN	56		/* memory management enable */
#define	TBIA	57		/* translation buffer invalidate all */
#define	TBIS	58		/* translation buffer invalidate single */
#define	PMR	61		/* performance monitor enable */
#define	SID	62		/* system identification */

/*
 * VAX-11/780 specific registers
 */
#define	ACCS	40		/* accelerator control and status */
#define	ACCR	41		/* accelerator maintenance */
#define	WCSA	44		/* WCS address */
#define	WCSD	45		/* WCS data */
#define	SBIFS	48		/* SBI fault and status */
#define	SBIS	49		/* SBI silo */
#define	SBISC	50		/* SBI silo comparator */
#define	SBIMT	51		/* SBI maintenance */
#define	SBIER	52		/* SBI error register */
#define	SBITA	53		/* SBI timeout address */
#define	SBIQC	54		/* SBI quadword clear */
#define	MBRK	60		/* micro-program breakpoint */
