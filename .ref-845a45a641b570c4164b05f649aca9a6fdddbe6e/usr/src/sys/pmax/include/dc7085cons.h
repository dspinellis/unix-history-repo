/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dc7085cons.h	7.3 (Berkeley) %G%
 *
 * dc7085.h --
 *
 *     	Definitions for the dc7085 chip.
 *
 *	Copyright (C) 1989 Digital Equipment Corporation.
 *	Permission to use, copy, modify, and distribute this software and
 *	its documentation for any purpose and without fee is hereby granted,
 *	provided that the above copyright notice appears in all copies.
 *	Digital Equipment Corporation makes no representations about the
 *	suitability of this software for any purpose.  It is provided "as is"
 *	without express or implied warranty.
 *
 * from: $Header: /sprite/src/kernel/dev/ds3100.md/RCS/dc7085.h,
 *	v 1.4 89/08/15 19:52:46 rab Exp $ SPRITE (DECWRL)
 */

#ifndef _DC7085
#define _DC7085

typedef volatile struct dc7085regs {
	u_short	dc_csr;		/* control and status (R/W) */
	u_short	pad0[3];
	short	dc_rbuf_lpr;	/* receiver data (R), line params (W) */
	u_short	pad1[3];
	u_short	dc_tcr;		/* transmitter control (R/W) */
	u_short	pad2[3];
	u_short	dc_msr_tdr;	/* modem status (R), transmit data (W) */
} dcregs;
#define dc_rbuf	dc_rbuf_lpr
#define dc_lpr	dc_rbuf_lpr
#define dc_msr	dc_msr_tdr
#define dc_tdr	dc_msr_tdr

/*
 * Control status register bits.
 */
#define	CSR_TRDY	0x8000
#define CSR_TIE		0x4000
#define	CSR_TX_LINE_NUM	0x0300
#define	CSR_RDONE	0x0080
#define	CSR_RIE		0x0040
#define CSR_MSE		0x0020
#define CSR_CLR		0x0010
#define CSR_MAINT	0x0008

/*
 * Receiver buffer register bits.
 */
#define	RBUF_DVAL		0x8000
#define RBUF_OERR		0x4000
#define RBUF_FERR		0x2000
#define RBUF_PERR		0x1000
#define RBUF_LINE_NUM		0x0300
#define RBUF_LINE_NUM_SHIFT	8
#define RBUF_CHAR		0x00FF

/*
 * Transmit control register values.
 */
#define TCR_DTR2		0x400
#define TCR_EN3			0x008
#define TCR_EN2			0x004
#define TCR_EN1			0x002
#define TCR_EN0			0x001

#define TCR_RTS2		0x800
#define TCR_RTS3		0x200
#define TCR_DTR3		0x100

/*
 * Line parameter register bits.
 */
#define	LPR_RXENAB	0x1000
#define LPR_B50		0x0000
#define LPR_B75		0x0100
#define LPR_B110	0x0200
#define LPR_B134	0x0300
#define LPR_B150	0x0400
#define LPR_B300	0x0500
#define LPR_B600	0x0600
#define LPR_B1200	0x0700
#define LPR_B1800	0x0800
#define LPR_B2000	0x0900
#define LPR_B2400	0x0A00
#define LPR_B3600	0x0B00
#define	LPR_B4800	0x0C00
#define LPR_B7200	0x0D00
#define LPR_B9600	0x0E00
#define LPR_B19200	0x0F00
#define LPR_B38400	0x0F00
#define LPR_OPAR	0x0080
#define LPR_PARENB	0x0040
#define LPR_2_STOP	0x0020
#define LPR_8_BIT_CHAR	0x0018
#define LPR_7_BIT_CHAR	0x0010
#define LPR_6_BIT_CHAR	0x0008
#define LPR_5_BIT_CHAR	0x0000

/*
 * Modem status register bits.
 */
#define	MSR_DSR2	0x0200

#define	MSR_RI2		0x0800
#define	MSR_CD2		0x0400
#define	MSR_CTS2	0x0100
#define	MSR_RI3		0x0008
#define	MSR_CD3		0x0004
#define	MSR_DSR3	0x0002
#define	MSR_CTS3	0x0001

/*
 * The four serial ports.
 */
#define DCKBD_PORT	0
#define DCMOUSE_PORT	1
#define DCCOMM_PORT	2
#define DCPRINTER_PORT	3

/* bits in dm lsr, copied from dmreg.h */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

#endif /* _DC7085 */
