/*	cons.h	3.2	%H%	*/

/*
 * VAX console interface registers
 */

#define	RXCS_IE		0x40		/* receiver interrupt enable */
#define	RXCS_DONE	0x80		/* receiver done */
#define	RXDB_DATA	0xff		/* received character */
#define	RXDB_ID		0xf00		/* channel id */
#define	RXDB_ERR	0x80000000	/* receiver error */

#define	TXCS_IE		0x40		/* transmitter interrupt enable */
#define	TXCS_RDY	0x80		/* transmitter ready for next char */
#define	TXDB_DATA	0xff		/* transmitter byte */
#define	TXDB_ID		0xf00		/* channel id */
#define	TXDB_AUTOR	0xf03		/* auto-restart command */
#define	TXDB_BOOT	0xf04		/* boot command */
