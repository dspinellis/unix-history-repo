/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mtpr.h	6.2 (Berkeley) %G%
 */

/*
 * VAX processor register numbers
 */

#define	KSP	0x0		/* kernel stack pointer */
#define	ESP	0x1		/* exec stack pointer */
#define	SSP	0x2		/* supervisor stack pointer */
#define	USP	0x3		/* user stack pointer */
#define	ISP	0x4		/* interrupt stack pointer */
#define	P0BR	0x8		/* p0 base register */
#define	P0LR	0x9		/* p0 length register */
#define	P1BR	0xa		/* p1 base register */
#define	P1LR	0xb		/* p1 length register */
#define	SBR	0xc		/* system segment base register */
#define	SLR	0xd		/* system segment length register */
#define	PCBB	0x10		/* process control block base */
#define	SCBB	0x11		/* system control block base */
#define	IPL	0x12		/* interrupt priority level */
#define	ASTLVL	0x13		/* async. system trap level */
#define	SIRR	0x14		/* software interrupt request */
#define	SISR	0x15		/* software interrupt summary */
#define	ICCS	0x18		/* interval clock control */
#define	NICR	0x19		/* next interval count */
#define	ICR	0x1a		/* interval count */
#define	TODR	0x1b		/* time of year (day) */
#define	RXCS	0x20		/* console receiver control and status */
#define	RXDB	0x21		/* console receiver data buffer */
#define	TXCS	0x22		/* console transmitter control and status */
#define	TXDB	0x23		/* console transmitter data buffer */
#define	MAPEN	0x38		/* memory management enable */
#define	TBIA	0x39		/* translation buffer invalidate all */
#define	TBIS	0x3a		/* translation buffer invalidate single */
#define	PMR	0x3d		/* performance monitor enable */
#define	SID	0x3e		/* system identification */

#if defined(VAX780)
#define	ACCS	0x28		/* accelerator control and status */
#define	ACCR	0x29		/* accelerator maintenance */
#define	WCSA	0x2c		/* WCS address */
#define	WCSD	0x2d		/* WCS data */
#define	SBIFS	0x30		/* SBI fault and status */
#define	SBIS	0x31		/* SBI silo */
#define	SBISC	0x32		/* SBI silo comparator */
#define	SBIMT	0x33		/* SBI maintenance */
#define	SBIER	0x34		/* SBI error register */
#define	SBITA	0x35		/* SBI timeout address */
#define	SBIQC	0x36		/* SBI quadword clear */
#define	MBRK	0x3c		/* micro-program breakpoint */
#endif

#if defined(VAX750) || defined(VAX730)
#define	MCSR	0x17		/* machine check status register */
#define	CSRS	0x1c		/* console storage receive status register */
#define	CSRD	0x1d		/* console storage receive data register */
#define	CSTS	0x1e		/* console storage transmit status register */
#define	CSTD	0x1f		/* console storage transmit data register */
#define	TBDR	0x24		/* translation buffer disable register */
#define	CADR	0x25		/* cache disable register */
#define	MCESR	0x26		/* machine check error summary register */
#define	CAER	0x27		/* cache error */
#define	IUR	0x37		/* init unibus register */
#define	TB	0x3b		/* translation buffer */
#endif
