/*-
 * Copyright (c) 1982, 1986, 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)mtpr.h	7.7 (Berkeley) %G%
 */
#ifndef _MTPR_H_
#define _MTPR_H_

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
#if VAX8200
#define	IPIR	0x16		/* interprocessor interrupt register */
#endif
#if VAX750 || VAX730
#define	MCSR	0x17		/* machine check status register */
#endif
#define	ICCS	0x18		/* interval clock control */
#define	NICR	0x19		/* next interval count */
#define	ICR	0x1a		/* interval count */
#if VAX8600 || VAX8200 || VAX780 || VAX750 || VAX730 || VAX650
#define	TODR	0x1b		/* time of year (day) */
#endif
#if VAX750 || VAX730
#define	CSRS	0x1c		/* console storage receive status register */
#define	CSRD	0x1d		/* console storage receive data register */
#define	CSTS	0x1e		/* console storage transmit status register */
#define	CSTD	0x1f		/* console storage transmit data register */
#endif
#define	RXCS	0x20		/* console receiver control and status */
#define	RXDB	0x21		/* console receiver data buffer */
#define	TXCS	0x22		/* console transmitter control and status */
#define	TXDB	0x23		/* console transmitter data buffer */
#if VAX8200 || VAX750 || VAX730 || VAX650
#define	TBDR	0x24		/* translation buffer disable register */
#define	CADR	0x25		/* cache disable register */
#endif
#if VAX8200 || VAX750 || VAX730
#define	MCESR	0x26		/* machine check error summary register */
#endif
#if VAX750 || VAX730 || VAX650
#define	CAER	0x27		/* cache error */
#endif
#define	ACCS	0x28		/* accelerator control and status */
#if VAX780
#define	ACCR	0x29		/* accelerator maintenance */
#endif
#if VAX8200 || VAX780
#define	WCSA	0x2c		/* WCS address */
#define	WCSD	0x2d		/* WCS data */
#endif
#if VAX8200
#define	WCSL	0x2e		/* WCS load */
#endif
#if VAX8600 || VAX780
#define	SBIFS	0x30		/* SBI fault and status */
#define	SBIS	0x31		/* SBI silo */
#define	SBISC	0x32		/* SBI silo comparator */
#define	SBIMT	0x33		/* SBI maintenance */
#define	SBIER	0x34		/* SBI error register */
#define	SBITA	0x35		/* SBI timeout address */
#define	SBIQC	0x36		/* SBI quadword clear */
#endif
#if VAX750 || VAX730 || VAX630 || VAX650
#define	IUR	0x37		/* init unibus (Qbus on 6x0) register */
#endif
#define	MAPEN	0x38		/* memory management enable */
#define	TBIA	0x39		/* translation buffer invalidate all */
#define	TBIS	0x3a		/* translation buffer invalidate single */
#if VAX750 || VAX730
#define	TB	0x3b		/* translation buffer */
#endif
#if VAX780
#define	MBRK	0x3c		/* micro-program breakpoint */
#endif
#define	PMR	0x3d		/* performance monitor enable */
#define	SID	0x3e		/* system identification */
#if VAX8600 || VAX8200 || VAX650
#define TBCHK	0x3f		/* Translation Buffer Check */
#endif
#if VAX8600
#define PAMACC	0x40		/* PAMM access */
#define PAMLOC	0x41		/* PAMM location */
#define CSWP	0x42		/* Cache sweep */
#define MDECC	0x43		/* MBOX data ecc register */
#define MENA	0x44		/* MBOX error enable register */
#define MDCTL	0x45		/* MBOX data control register */
#define MCCTL	0x46		/* MBOX mcc control register */
#define MERG	0x47		/* MBOX	error generator register */
#define CRBT	0x48		/* Console reboot */
#define DFI	0x49		/* Diag fault insertion register */
#define EHSR	0x4a		/* Error handling status register */
#define STXCS	0x4c		/* Console block storage C/S */
#define STXDB	0x4d		/* Console block storage D/B */
#define ESPA	0x4e		/* EBOX scratchpad address */
#define ESPD	0x4f		/* EBOX sratchpad data */
#endif
#if VAX8200
#define	RXCS1	0x50		/* receive csr, console line 1 */
#define	RXDB1	0x51		/* receive data buffer, console line 1 */
#define	TXCS1	0x52		/* transmit csr, console line 1 */
#define	TXDB1	0x53		/* transmit data buffer, console line 1 */
#define	RXCS2	0x54		/* etc */
#define	RXDB2	0x55
#define	TXCS2	0x56
#define	TXDB2	0x57
#define	RXCS3	0x58
#define	RXDB3	0x59
#define	TXCS3	0x5a
#define	TXDB3	0x5b
#define	RXCD	0x5c		/* receive console data register */
#define	CACHEX	0x5d		/* cache invalidate register */
#define	BINID	0x5e		/* VAXBI node ID register */
#define	BISTOP	0x5f		/* VAXBI stop register */
#endif
#endif /*_MTPR_H_*/
