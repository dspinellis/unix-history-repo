/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cons.h	6.2 (Berkeley) %G%
 */

/*
 * VAX console interface registers
 */

#define	RXCS_IE		0x00000040	/* receiver interrupt enable */
#define	RXCS_DONE	0x00000080	/* receiver done */

#define	RXDB_DATA	0x000000ff	/* received character */
#define	RXDB_ID		0x00000f00	/* channel id */
#define	RXDB_ERR	0x80000000	/* receiver error */

#define	TXCS_IE		0x00000040	/* transmitter interrupt enable */
#define	TXCS_RDY	0x00000080	/* transmitter ready for next char */
#define	TXDB_DATA	0x000000ff	/* transmitter byte */
#define	TXDB_ID		0x00000f00	/* channel id */

#define	TXDB_DONE	0xf01		/* software done */
#define	TXDB_BOOT	0xf02		/* reboot */
#define	TXDB_CWSI	0xf03		/* clear warm start inhibit */
#define	TXDB_CCSI	0xf04		/* clear cold-start inhibit */
