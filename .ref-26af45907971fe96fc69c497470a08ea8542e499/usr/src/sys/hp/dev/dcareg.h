/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dcareg.h	7.5 (Berkeley) %G%
 */

#include <hp/dev/iotypes.h>			/* XXX */

#ifdef hp700
struct dcadevice {
	vu_char	dca_reset;
	vu_char dca_pad[0x800-1];
	vu_char	dca_data;			/* receive buf or xmit hold */
	vu_char	dca_ier;			/* interrupt enable */
	vu_char	dca_iir;			/* (RO) interrupt identify */
#define		dca_fifo	dca_iir		/* (WO) FIFO control */
	vu_char	dca_cfcr;			/* line control */
	vu_char	dca_mcr;			/* modem control */
	vu_char	dca_lsr;			/* line status */
	vu_char	dca_msr;			/* modem status */
	vu_char	dca_scr;			/* scratch pad */
};
#else
struct dcadevice {
	/* card registers */
	u_char	dca_pad0;
	vu_char	dca_id;				/* 0x01 (read) */
#define		dca_reset	dca_id		/* 0x01 (write) */
	u_char	dca_pad1;
	vu_char	dca_ic;				/* 0x03 */
	u_char	dca_pad2;
	vu_char	dca_ocbrc;			/* 0x05 */
	u_char	dca_pad3;
	vu_char	dca_lcsm;			/* 0x07 */
	u_char	dca_pad4[8];
	/* chip registers */
	u_char	dca_pad5;
	vu_char	dca_data;			/* 0x11 */
	u_char	dca_pad6;
	vu_char	dca_ier;			/* 0x13 */
	u_char	dca_pad7;
	vu_char	dca_iir;			/* 0x15 (read) */
#define		dca_fifo	dca_iir		/* 0x15 (write) */
	u_char	dca_pad8;
	vu_char	dca_cfcr;			/* 0x17 */
	u_char	dca_pad9;
	vu_char	dca_mcr;			/* 0x19 */
	u_char	dca_padA;
	vu_char	dca_lsr;			/* 0x1B */
	u_char	dca_padB;
	vu_char	dca_msr;			/* 0x1D */
};
#endif

/* interface reset/id (300 only) */
#define	DCAID0		0x02
#define DCAREMID0	0x82
#define	DCAID1		0x42
#define DCAREMID1	0xC2

/* interrupt control (300 only) */
#define	DCAIPL(x)	((((x) >> 4) & 3) + 3)
#define	IC_IR		0x40
#define	IC_IE		0x80

/*
 * 16 bit baud rate divisor (lower byte in dca_data, upper in dca_ier)
 * NB: This constant is for a 7.3728 clock frequency. The 300 clock
 *     frequency is 2.4576, giving a constant of 153600.
 */
#ifdef hp300
#define	DCABRD(x)	(153600 / (x))
#endif
#ifdef hp700
#define	DCABRD(x)	(460800 / (x))
#endif

/* interrupt enable register */
#define	IER_ERXRDY	0x1
#define	IER_ETXRDY	0x2
#define	IER_ERLS	0x4
#define	IER_EMSC	0x8

/* interrupt identification register */
#define	IIR_IMASK	0xf
#define	IIR_RXTOUT	0xc
#define	IIR_RLS		0x6
#define	IIR_RXRDY	0x4
#define	IIR_TXRDY	0x2
#define	IIR_NOPEND	0x1
#define	IIR_MLSC	0x0
#define	IIR_FIFO_MASK	0xc0	/* set if FIFOs are enabled */

/* fifo control register */
#define	FIFO_ENABLE	0x01
#define	FIFO_RCV_RST	0x02
#define	FIFO_XMT_RST	0x04
#define	FIFO_DMA_MODE	0x08
#define	FIFO_TRIGGER_1	0x00
#define	FIFO_TRIGGER_4	0x40
#define	FIFO_TRIGGER_8	0x80
#define	FIFO_TRIGGER_14	0xc0

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
#define	MCR_IEN		0x08
#define	MCR_DRS		0x04
#define	MCR_RTS		0x02
#define	MCR_DTR		0x01

/* line status register */
#define	LSR_RCV_FIFO	0x80
#define	LSR_TSRE	0x40
#define	LSR_TXRDY	0x20
#define	LSR_BI		0x10
#define	LSR_FE		0x08
#define	LSR_PE		0x04
#define	LSR_OE		0x02
#define	LSR_RXRDY	0x01
#define	LSR_RCV_MASK	0x1f

/* modem status register */
#define	MSR_DCD		0x80
#define	MSR_RI		0x40
#define	MSR_DSR		0x20
#define	MSR_CTS		0x10
#define	MSR_DDCD	0x08
#define	MSR_TERI	0x04
#define	MSR_DDSR	0x02
#define	MSR_DCTS	0x01

#ifdef hp300
/* WARNING: Serial console is assumed to be at SC9 */
#define CONSCODE	(9)
#endif
#ifdef hp700
/* hardwired port addresses */
#define PORT1		((struct dcadevice *)CORE_RS232_1)
#define PORT2		((struct dcadevice *)CORE_RS232_2)
#define CONPORT		PORT1
#endif
#define CONUNIT		(0)
