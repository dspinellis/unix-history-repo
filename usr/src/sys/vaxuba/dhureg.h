/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dhureg.h	7.1 (Berkeley) 6/5/86
 */

/* 
 * DHU-11 device register definitions.
 */
struct dhudevice {
	union {
		short	csr;		/* control-status register */
		struct {
			char	csrl;	/* low byte for line select */
			char	csrh;	/* high byte for tx line */
		} cb;
	} un1;
#define	dhucsr	un1.csr
#define	dhucsrl	un1.cb.csrl
#define	dhucsrh	un1.cb.csrh
	union {
		short	rbuf;		/* recv.char/ds.change register (R) */
		short	timo;		/* delay between recv -> intr (W) */
	} un2;
#define	dhurbuf	un2.rbuf
#define	dhutimo	un2.timo
	short	dhulpr;			/* line parameter register */
	union {
		char	fbyte[1];	/* fifo data byte (low byte only) (W) */
		short	fdata;		/* fifo data word (W) */
		char	sbyte[2];	/* line status/fifo size (R) */
	} un3;
#define	dhubyte	un3.fbyte[0]
#define dhufifo	un3.fdata
#define dhusize	un3.sbyte[0]
#define dhustat	un3.sbyte[1]
	short	dhulcr;			/* line control register */
	short	dhubar1;		/* buffer address register 1 */
	char	dhubar2;		/* buffer address register 2 */
	char	dhulcr2;		/* xmit enable bit */
	short	dhubcr;			/* buffer count register */
};

/* Bits in dhucsr */
#define	DHU_CS_TIE	0x4000		/* transmit interrupt enable */
#define	DHU_CS_DFAIL	0x2000		/* diagnostic fail */
#define	DHU_CS_RI	0x0080		/* receiver interrupt */
#define	DHU_CS_RIE	0x0040		/* receiver interrupt enable */
#define	DHU_CS_MCLR	0x0020		/* master clear */
#define	DHU_CS_SST	0x0010		/* skip self test (with DHU_CS_MCLR) */
#define	DHU_CS_IAP	0x000f		/* indirect address pointer */

#define	DHU_IE	(DHU_CS_TIE|DHU_CS_RIE)

/* map unit into iap register select */
#define DHU_SELECT(unit)	((unit) & DHU_CS_IAP)

/* Transmitter bits in high byte of dhucsr */
#define	DHU_CSH_TI	0x80		/* transmit interrupt */
#define	DHU_CSH_NXM	0x10		/* transmit dma err: non-exist-mem */
#define	DHU_CSH_TLN	0x0f		/* transmit line number */

/* map csrh line bits into line */
#define	DHU_TX_LINE(csrh)	((csrh) & DHU_CSH_TLN)

/* Bits in dhurbuf */
#define	DHU_RB_VALID	0x8000		/* data valid */
#define	DHU_RB_STAT	0x7000		/* status bits */
#define	DHU_RB_DO	0x4000		/* data overrun */
#define	DHU_RB_FE	0x2000		/* framing error */
#define	DHU_RB_PE	0x1000		/* parity error */
#define	DHU_RB_RLN	0x0f00		/* receive line number */
#define	DHU_RB_RDS	0x00ff		/* receive data/status */
#define DHU_RB_DIAG	0x0001		/* if DHU_RB_STAT -> diag vs modem */

/* map rbuf line bits into line */
#define	DHU_RX_LINE(rbuf)	(((rbuf) & DHU_RB_RLN) >> 8)

/* Bits in dhulpr */
#define	DHU_LP_TSPEED	0xf000
#define	DHU_LP_RSPEED	0x0f00
#define	DHU_LP_TWOSB	0x0080
#define	DHU_LP_EPAR	0x0040
#define	DHU_LP_PENABLE	0x0020
#define	DHU_LP_BITS8	0x0018
#define	DHU_LP_BITS7	0x0010
#define	DHU_LP_BITS6	0x0008

/* Bits in dhustat */
#define	DHU_ST_DSR	0x80		/* data set ready */
#define	DHU_ST_RI	0x20		/* ring indicator */
#define	DHU_ST_DCD	0x10		/* carrier detect */
#define	DHU_ST_CTS	0x04		/* clear to send */
#define	DHU_ST_DHU	0x01		/* always one on a dhu, zero on dhv */

/* Bits in dhulcr */
#define	DHU_LC_RTS	0x1000		/* request to send */
#define	DHU_LC_DTR	0x0200		/* data terminal ready */
#define	DHU_LC_MODEM	0x0100		/* modem control enable */
#define	DHU_LC_MAINT	0x00c0		/* maintenance mode */
#define	DHU_LC_FXOFF	0x0020		/* force xoff */
#define	DHU_LC_OAUTOF	0x0010		/* output auto flow */
#define	DHU_LC_BREAK	0x0008		/* break control */
#define	DHU_LC_RXEN	0x0004		/* receiver enable */
#define	DHU_LC_IAUTOF	0x0002		/* input auto flow */
#define	DHU_LC_TXABORT	0x0001		/* transmitter abort */

/* Bits in dhulcr2 */
#define	DHU_LC2_TXEN	0x80		/* transmitter enable */

/* Bits in dhubar2 */
#define	DHU_BA2_DMAGO	0x80		/* transmit dma start */
#define	DHU_BA2_XBA	0x03		/* top two bits of dma address */
#define DHU_XBA_SHIFT	16		/* amount to shift xba bits */

/* Bits for dhumctl only:  stat bits are shifted up 16 */
#define	DHU_ON	(DHU_LC_DTR|DHU_LC_RTS|DHU_LC_MODEM)
#define	DHU_OFF	DHU_LC_MODEM

#define	DHU_DSR	(DHU_ST_DSR << 16)
#define	DHU_RNG	(DHU_ST_RI << 16)
#define	DHU_CAR	(DHU_ST_DCD << 16)
#define	DHU_CTS	(DHU_ST_CTS << 16)

#define	DHU_RTS	DHU_LC_RTS
#define	DHU_DTR	DHU_LC_DTR
#define DHU_BRK	DHU_LC_BREAK
#define DHU_LE	DHU_LC_MODEM

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
