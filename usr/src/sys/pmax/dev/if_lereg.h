/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_lereg.h	7.1 (Berkeley) %G%
 */

#define	LEMTU		1518
#define	LEMINSIZE	60	/* should be 64 if mode DTCR is set */
#define	LERBUF		32
#define	LERBUFLOG2	5
#define	LE_RLEN		(LERBUFLOG2 << 13)
#define	LETBUF		8
#define	LETBUFLOG2	3
#define	LE_TLEN		(LETBUFLOG2 << 13)

/*
 * LANCE registers.
 */
struct lereg1 {
	u_short	ler1_rdp;	/* data port */
	short	pad0;
	u_short	ler1_rap;	/* register select port */
	short	pad1;
};

/*
 * Overlayed on 64Kbyte dual-port RAM.
 * Currently 32 * 1518 receive plus 8 * 1518 transmit buffers plus
 * buffer descriptor rings.
 */
struct lereg2 {
	/* init block */		/* CHIP address */
	u_short	ler2_mode;		/* +0x0000 */
	short	pad0;
	u_short	ler2_padr0;		/* +0x0002 */
	short	pad1;
	u_short	ler2_padr1;		/* +0x0004 */
	short	pad2;
	u_short	ler2_padr2;		/* +0x0006 */
	short	pad3;
	u_short	ler2_ladrf0;		/* +0x0008 */
	short	pad4;
	u_short	ler2_ladrf1;		/* +0x000A */
	short	pad5;
	u_short	ler2_ladrf2;		/* +0x000C */
	short	pad6;
	u_short	ler2_ladrf3;		/* +0x000E */
	short	pad7;
	u_short	ler2_rdra;		/* +0x0010 */
	short	pad8;
	u_short	ler2_rlen;		/* +0x0012 */
	short	pad9;
	u_short	ler2_tdra;		/* +0x0014 */
	short	pad10;
	u_short	ler2_tlen;		/* +0x0016 */
	short	pad11;
	/* receive message descriptors */
	struct	lermd {			/* +0x0018 */
		u_short	rmd0;
		short	pad0;
		u_short	rmd1;
		short	pad1;
		short	rmd2;
		short	pad2;
		u_short	rmd3;
		short	pad3;
	} ler2_rmd[LERBUF];
	/* transmit message descriptors */
	struct	letmd {			/* +0x0058 */
		u_short	tmd0;
		short	pad0;
		u_short	tmd1;
		short	pad1;
		short	tmd2;
		short	pad2;
		u_short	tmd3;
		short	pad3;
	} ler2_tmd[LETBUF];
	u_short	ler2_rbuf[LERBUF][LEMTU]; /* +0x0060 */
	u_short	ler2_tbuf[LETBUF][LEMTU]; /* +0x2FD0 */
};

/*
 * Control and status bits -- lereg0
 */
#define	LE_IE		0x80		/* interrupt enable */
#define	LE_IR		0x40		/* interrupt requested */
#define	LE_LOCK		0x08		/* lock status register */
#define	LE_ACK		0x04		/* ack of lock */
#define	LE_JAB		0x02		/* loss of tx clock (???) */
#define LE_IPL(x)	((((x) >> 4) & 0x3) + 3)

/*
 * Control and status bits -- lereg1
 */
#define	LE_CSR0		0
#define	LE_CSR1		1
#define	LE_CSR2		2
#define	LE_CSR3		3

#define	LE_SERR		0x8000
#define	LE_BABL		0x4000
#define	LE_CERR		0x2000
#define	LE_MISS		0x1000
#define	LE_MERR		0x0800
#define	LE_RINT		0x0400
#define	LE_TINT		0x0200
#define	LE_IDON		0x0100
#define	LE_INTR		0x0080
#define	LE_INEA		0x0040
#define	LE_RXON		0x0020
#define	LE_TXON		0x0010
#define	LE_TDMD		0x0008
#define	LE_STOP		0x0004
#define	LE_STRT		0x0002
#define	LE_INIT		0x0001

#define	LE_BSWP		0x4
#define	LE_MODE		0x0

/*
 * Control and status bits -- lereg2
 */
#define	LE_OWN		0x8000
#define	LE_ERR		0x4000
#define	LE_STP		0x0200
#define	LE_ENP		0x0100

#define	LE_FRAM		0x2000
#define	LE_OFLO		0x1000
#define	LE_CRC		0x0800
#define	LE_RBUFF	0x0400

#define	LE_MORE		0x1000
#define	LE_ONE		0x0800
#define	LE_DEF		0x0400

#define	LE_TBUFF	0x8000
#define	LE_UFLO		0x4000
#define	LE_LCOL		0x1000
#define	LE_LCAR		0x0800
#define	LE_RTRY		0x0400
