/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dcareg.h	7.2 (Berkeley) %G%
 */

struct dcadevice {
	u_char	dca_pad0;
	volatile u_char	dca_irid;
	volatile short	dca_ic;
	volatile short	dca_ocbrc;
	volatile short	dca_lcsm;
	short	dca_pad1[4];
	u_char	dca_pad2;
	volatile u_char	dca_data;
	volatile short	dca_ier;
	u_char	dca_pad4;
	volatile u_char	dca_iir;
	volatile short	dca_cfcr;
	volatile short	dca_mcr;
	volatile short	dca_lsr;
	u_char	dca_pad3;
	volatile u_char	dca_msr;
};

/* interface reset/id */
#define	DCAID0		0x02
#define DCAREMID0	0x82
#define	DCAID1		0x42
#define DCAREMID1	0xC2

/* interrupt control */
#define	DCAIPL(x)	((((x) >> 4) & 3) + 3)
#define	IC_IR		0x40
#define	IC_IE		0x80

/* 16 bit baud rate divisor (lower byte in dca_data, upper in dca_ier) */
#define	DCABRD(x)	(153600 / (x))

/* interrupt enable register */
#define	IER_ERXRDY	0x1
#define	IER_ETXRDY	0x2
#define	IER_ERLS	0x4
#define	IER_EMSC	0x8

/* interrupt identification register */
#define	IIR_NOPEND	0x1
#define	IIR_IMASK	0x6
#define	IIR_RLS		6
#define	IIR_RXRDY	4
#define	IIR_TXRDY	2
#define	IIR_MLSC	0

/* character format control register */
#define	CFCR_DLAB	0x80
#define	CFCR_SBREAK	0x40
#define	CFCR_PZERO	0x30
#define	CFCR_PONE	0x20
#define	CFCR_PEVEN	0x10
#define	CFCR_PODD	0x00
#define	CFCR_PENAB	0x08
#define	CFCR_STOPB	0x04
#define	CFCR_8BITS	0x03
#define	CFCR_7BITS	0x02
#define	CFCR_6BITS	0x01
#define	CFCR_5BITS	0x00

/* modem control register */
#define	MCR_LOOPBACK	0x10
#define	MCR_SRTS	0x08
#define	MCR_DRS		0x04
#define	MCR_RTS		0x02
#define	MCR_DTR		0x01

/* line status register */
#define	LSR_TSRE	0x40
#define	LSR_TXRDY	0x20
#define	LSR_BI		0x10
#define	LSR_FE		0x08
#define	LSR_PE		0x04
#define	LSR_OE		0x02
#define	LSR_RXRDY	0x01

/* modem status register */
#define	MSR_DCD		0x80
#define	MSR_RI		0x40
#define	MSR_DSR		0x20
#define	MSR_CTS		0x10
#define	MSR_DDCD	0x08
#define	MSR_TERI	0x04
#define	MSR_DDSR	0x02
#define	MSR_DCTS	0x01

/*
 * WARNING: Serial console is assumed to be at SC9
 * and CONUNIT must be 0.
 */
#define CONADDR	((struct dcadevice *)(IOV(EXTIOBASE + (9 * IOCARDSIZE))))
#define CONUNIT	(0)
