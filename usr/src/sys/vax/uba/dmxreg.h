/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmxreg.h	7.1 (Berkeley) 9/4/89
 *
 * Common structures and definitions
 * for merged DMF and DMZ drivers.
 */

/*
 * Hardware registers per octet of asynchronous lines
 */
struct dmx_octet {
	short	csr;			/* control-status register */
	short	lpr;			/* line parameter register */
	short	rbuf;			/* receiver buffer (ro) */
	union {
		u_short	irw;		/* indirect register word */
		u_char	irc[2];		/*    "         "    bytes */
	} octun;
};

#define	rsp	rbuf		/* receive silo parameter register (wo) */
#define	tbuf	octun.irc[0]	/* transmit buffer */
#define	tsc	octun.irc[0]	/* transmit silo count */
#define	rmstsc	octun.irw	/* rcv modem status, xmit silo count */
#define	rms	octun.irc[1]	/* receive modem status */
#define	lctms	octun.irw	/* line control, transmit modem status */
#define	tba	octun.irw	/* transmit buffer address */
#define	tcc	octun.irw	/* transmit character count */

/* bits in dmfcsr */
#define	DMF_TI	0100000		/* transmit interrupt */
#define	DMF_TIE	0040000		/* transmit interrupt enable */
#define	DMF_NXM	0030000		/* non-existent memory (which bit?) */
#define	DMF_LIN	0003400		/* transmit line number */
#define	DMF_RI	0000200		/* receiver interrupt */
#define	DMF_RIE	0000100		/* receiver interrupt enable */
#define	DMF_CLR	0000040		/* master reset */
#define	DMF_IAD	0000037		/* indirect address register */

#define	DMF_IE	(DMF_TIE|DMF_RIE)

#define	DMFIR_RMSTSC	000	/* select rmstsc indirect register */
#define	DMFIR_TBUF	000	/* select tbuf indirect register */
#define	DMFIR_LCR	010	/* select lcr indirect register */
#define	DMFIR_TBA	020	/* select tba indirect register */
#define	DMFIR_TCC	030	/* select tcc indirect register */

/* bits in dmflpr */
#define	BITS6		0010	/* 6 bits per character */
#define	BITS7		0020	/* 7 bits per character */
#define	BITS8		0030	/* 8 bits per character */
#define	PENABLE		0040	/* parity enable */
#define	EPAR		0100	/* even parity */
#define	TWOSB		0200	/* two stop bits */

#define	DMF_SILOCNT	32		/* size of DMF output silo (per line) */

/* bits in dmfrbuf */
#define	DMF_DSC		0004000		/* data set change */
#define	DMF_PE		0010000		/* parity error */
#define	DMF_FE		0020000		/* framing error */
#define	DMF_DO		0040000		/* data overrun */

/* bits in dmfrmstsc */
#define	DMF_TSC		0x00ff		/* transmit silo count */
#define	DMF_USRR	0x0400		/* user modem signal (pin 25) */
#define	DMF_SR		0x0800		/* secondary receive */
#define	DMF_CTS		0x1000		/* clear to send */
#define	DMF_CAR		0x2000		/* carrier detect */
#define	DMF_RNG		0x4000		/* ring */
#define	DMF_DSR		0x8000		/* data set ready */

/* bits in dmflctms (tms half) */
#define	DMF_USRW	0x0100		/* user modem signal (pin 18) */
#define	DMF_DTR		0x0200		/* data terminal ready */
#define	DMF_RATE	0x0400		/* data signal rate select */
#define	DMF_SRTS	0x0800		/* secondary request to send (dmf) */
#define	DMF_RTS		0x1000		/* request to send */
#define	DMF_PREEMPT	0x8000		/* preempt output */

/* bits in dmflctms (lc half) */
#define	DMF_MIE		0040		/* modem interrupt enable */
#define	DMF_FLUSH	0020		/* flush transmit silo */
#define	DMF_BRK		0010		/* send break bit */
#define	DMF_RE		0004		/* receive enable */
#define	DMF_AUTOX	0002		/* auto XON/XOFF */
#define	DMF_TE		0001		/* transmit enable */

#define	DMF_ENA		(DMF_MIE|DMF_RE|DMF_TE)

/* flags for modem control */
#define	DMF_ON		(DMF_DTR|DMF_RTS|DMF_ENA)
#define	DMF_OFF		0

/* bits added to dm lsr for DMGET/DMSET */
#define	DML_USR		0001000		/* usr modem sig, not a real DM bit */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
