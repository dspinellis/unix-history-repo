/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dzreg.h	7.1 (Berkeley) 6/5/86
 */

/*
 * DZ-11/DZ-32 Registers and bits.
 */
struct dzdevice {
	short dzcsr;
	short dzrbuf;
	union {
		struct {
			char	dztcr0;
			char	dzdtr0;
			char	dztbuf0;
			char	dzbrk0;
		} dz11;
		struct {
			short	dzlcs0;
			char	dztbuf0;
			char	dzlnen0;
		} dz32;
	} dzun;
};

#define dzlpr	dzrbuf
#define dzmsr	dzun.dz11.dzbrk0
#define dztcr	dzun.dz11.dztcr0
#define dzdtr	dzun.dz11.dzdtr0
#define dztbuf	dzun.dz11.dztbuf0
#define dzlcs	dzun.dz32.dzlcs0
#define	dzbrk	dzmsr
#define dzlnen	dzun.dz32.dzlnen0
#define dzmtsr	dzun.dz32.dztbuf0

/* bits in dzlpr */
#define	BITS7	0020
#define	BITS8	0030
#define	TWOSB	0040
#define	PENABLE	0100
#define	OPAR	0200

/* bits in dzrbuf */
#define	DZ_PE	010000
#define	DZ_FE	020000
#define	DZ_DO	040000

/* bits in dzcsr */
#define	DZ_32	000001		/* DZ32 mode */
#define	DZ_MIE	000002		/* Modem Interrupt Enable */
#define	DZ_CLR	000020		/* Reset dz */
#define	DZ_MSE	000040		/* Master Scan Enable */
#define	DZ_RIE	000100		/* Receiver Interrupt Enable */
#define DZ_MSC	004000		/* Modem Status Change */
#define	DZ_SAE	010000		/* Silo Alarm Enable */
#define	DZ_TIE	040000		/* Transmit Interrupt Enable */
#define	DZ_IEN	(DZ_32|DZ_MIE|DZ_MSE|DZ_RIE|DZ_TIE)

/* flags for modem-control */
#define	DZ_ON	DZ_DTR
#define	DZ_OFF	0

/* bits in dzlcs */
#define DZ_ACK	0100000		/* ACK bit in dzlcs */
#define DZ_RTS	0010000		/* Request To Send */
#define	DZ_ST	0004000		/* Secondary Transmit */
#define	DZ_BRK	0002000		/* Break */
#define DZ_DTR	0001000		/* Data Terminal Ready */
#define	DZ_LE	0000400		/* Line Enable */
#define	DZ_DSR	0000200		/* Data Set Ready */
#define	DZ_RI	0000100		/* Ring Indicate */
#define DZ_CD	0000040		/* Carrier Detect */
#define	DZ_CTS	0000020		/* Clear To Send */
#define	DZ_SR	0000010		/* Secondary Receive */
 
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
