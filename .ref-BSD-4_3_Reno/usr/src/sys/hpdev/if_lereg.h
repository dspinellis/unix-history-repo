/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_lereg.h	7.1 (Berkeley) 5/8/90
 */

#define	LEID		21

#define	LEMTU		1518
#define	LEMINSIZE	60	/* should be 64 if mode DTCR is set */
#define	LERBUF		8
#define	LERBUFLOG2	3
#define	LE_RLEN		(LERBUFLOG2 << 13)
#define	LETBUF		1
#define	LETBUFLOG2	0
#define	LE_TLEN		(LETBUFLOG2 << 13)

#define vu_char		volatile u_char

/*
 * LANCE registers.
 */
struct lereg0 {
	u_char	ler0_pad0;
	vu_char	ler0_id;	/* ID */
	u_char	ler0_pad1;
	vu_char	ler0_status;	/* interrupt enable/status */
};

struct lereg1 {
	u_short	ler1_rdp;	/* data port */
	u_short	ler1_rap;	/* register select port */
};

/*
 * Overlayed on 16K dual-port RAM.
 * Current size is 13,758 bytes with 8 x 1518 receive buffers and
 * 1 x 1518 transmit buffer.
 */
struct lereg2 {
	/* init block */
	u_short	ler2_mode;		/* +0x0000 */
	u_char	ler2_padr[6];		/* +0x0002 */
	u_long	ler2_ladrf0;		/* +0x0008 */
	u_long	ler2_ladrf1;		/* +0x000C */
	u_short	ler2_rdra;		/* +0x0010 */
	u_short	ler2_rlen;		/* +0x0012 */
	u_short	ler2_tdra;		/* +0x0014 */
	u_short	ler2_tlen;		/* +0x0016 */
	/* receive message descriptors */
	struct	lermd {			/* +0x0018 */
		u_short	rmd0;
		u_short	rmd1;
		short	rmd2;
		u_short	rmd3;
	} ler2_rmd[LERBUF];
	/* transmit message descriptors */
	struct	letmd {			/* +0x0058 */
		u_short	tmd0;
		u_short	tmd1;
		short	tmd2;
		u_short	tmd3;
	} ler2_tmd[LETBUF];
	char	ler2_rbuf[LERBUF][LEMTU]; /* +0x0060 */
	char	ler2_tbuf[LETBUF][LEMTU]; /* +0x2FD0 */
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
