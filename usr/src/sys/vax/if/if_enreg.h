/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_enreg.h	7.3 (Berkeley) %G%
 */

/*
 * Xerox experimental ethernet registers.
 *
 * N.B.: status register and device address are read/write,
 * device address is read-only, rest are WRITE ONLY!
 */
struct endevice {
	short	en_owc;		/* output word count (10 bits) */
	short	en_oba;		/* output buffer address */
	short	en_ostat;	/* output control and status */
	short	en_odelay;	/* output start delay, 25usec units  */
	short	en_iwc;		/* input word count */
	short	en_iba;		/* input buffer address */
	short	en_istat;	/* input csr */
	short	en_addr;	/* ~device address (low 8 bits) */
};

/*
 * Control and status bits.
 */
#define EN_IERROR	0x8000		/* CRC error, buf ovflo or overrun */
#define	EN_OERROR	0x8000		/* collision or output underrun */
#define EN_OPDONE	0x0080		/* previous operation completed */
#define EN_IEN		0x0040		/* enable interrupt when DONE */
#define	EN_PROMISCUOUS	0x0002		/* promiscuous, input any packet */
#define EN_GO		0x0001		/* start op bit */

#define	EN_BITS	"\10\20ERR\10OPDONE\7IEN\2PROM\1GO"

#define	spl_enet()	spl5()
