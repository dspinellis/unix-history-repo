/*	dmfreg.h	6.1	83/07/29	*/

/*
 * DMF-32 definitions.
 */

/*
 * "dmf" (unqualified) refers to the async portion of the dmf32,
 * "dmfc" to the combo portion,
 * "dmfs" to the sync portion,
 * "dmfl" to the lp portion, and
 * "dmfd" to the dr portion.
 */
struct dmfdevice {
	short	dmfccsr0;		/* combo csr 0 */
	short	dmfccsr1;		/* combo csr 1 */
	short	dmfs[4];
	short	dmfcsr;			/* control-status register */
	short	dmflpr;			/* line parameter register */
	short	dmfrbuf;		/* receiver buffer (ro) */
	union {
		u_short	dmfirw;		/* indirect register word */
		u_char	dmfirc[2];	/*    "         "    bytes */
	} dmfun;
	short	dmfl[2];
	short	dmfd[4];
};

#define	dmfrsp	dmfrbuf		/* receive silo parameter register (wo) */
#define	dmftbuf	dmfun.dmfirc[0]	/* transmit buffer */
#define	dmftsc	dmfun.dmfirc[0]	/* transmit silo count */
#define	dmfrms	dmfun.dmfirc[1]	/* receive modem status */
#define	dmflcr	dmfun.dmfirc[0]	/* line control register */
#define	dmftms	dmfun.dmfirc[1]	/* transmit modem status */
#define	dmftba	dmfun.dmfirw	/* transmit buffer address */
#define	dmftcc	dmfun.dmfirw	/* transmit character count */

/* bits in dmfcsr */
#define	DMF_TI	0100000		/* transmit interrupt */
#define	DMF_TIE	0040000		/* transmit interrupt enable */
#define	DMF_NXM	0020000		/* non-existant memory */
#define	DMF_LIN	0003400		/* transmit line number */
#define	DMF_RI	0000200		/* receiver interrupt */
#define	DMF_RIE	0000100		/* receiver interrupt enable */
#define	DMF_CLR	0000040		/* master reset */
#define	DMF_IAD	0000037		/* indirect address register */

#define	DMFIR_TBUF	000	/* select tbuf indirect register */
#define	DMFIR_LCR	010	/* select lcr indirect register */
#define	DMFIR_TBA	020	/* select tba indirect register */
#define	DMFIR_TCC	030	/* select tcc indirect register */

/* bits in dmflpr */
#define	BITS6	(01<<3)
#define	BITS7	(02<<3)
#define	BITS8	(03<<3)
#define	TWOSB	0200
#define	PENABLE	040
#define	EPAR	0100

#define	DMF_IE	(DMF_TIE|DMF_RIE)

#define	DMF_SILOCNT	32		/* size of DMF output silo (per line) */

/* bits in dmfrbuf */
#define	DMF_DSC		0004000		/* data set change */
#define	DMF_PE		0010000		/* parity error */
#define	DMF_FE		0020000		/* framing error */
#define	DMF_DO		0040000		/* data overrun */

/* bits in dmfrms */
#define	DMF_USRR	0004		/* user modem signal (pin 25) */
#define	DMF_SR		0010		/* secondary receive */
#define	DMF_CTS		0020		/* clear to send */
#define	DMF_CAR		0040		/* carrier detect */
#define	DMF_RNG		0100		/* ring */
#define	DMF_DSR		0200		/* data set ready */

/* bits in dmftms */
#define	DMF_USRW	0001		/* user modem signal (pin 18) */
#define	DMF_DTR		0002		/* data terminal ready */
#define	DMF_RATE	0004		/* data signal rate select */
#define	DMF_ST		0010		/* secondary transmit */
#define	DMF_RTS		0020		/* request to send */
#define	DMF_BRK		0040		/* pseudo break bit */
#define	DMF_PREEMPT	0200		/* preempt output */

/* flags for modem control */
#define	DMF_ON	(DMF_DTR|DMF_RTS)
#define	DMF_OFF	0

/* bits in dmflcr */
#define	DMF_MIE		0040		/* modem interrupt enable */
#define	DMF_FLUSH	0020		/* flush transmit silo */
#define	DMF_RBRK	0010		/* real break bit */
#define	DMF_RE		0004		/* receive enable */
#define	DMF_AUTOX	0002		/* auto XON/XOFF */
#define	DMF_TE		0001		/* transmit enable */

#define	DMFLCR_ENA	(DMF_MIE|DMF_RE|DMF_TE)

/* bits in dm lsr, copied from dh.c */
#define	DML_USR		0001000		/* usr modem sig, not a real DM bit */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

#define SETLCR(pt, exp) \
	pt->dmfun.dmfirw = (((pt)->dmftms)<<8) | ((exp)&0xff)
