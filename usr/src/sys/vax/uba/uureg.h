
/*	uureg.h	4.1	83/04/09	*/

/*
 * TU58 UNIBUS controller registers
 */
struct tudevice {
	short	turcs;	/* receiver status register */
	short	turdb;	/* receiver data buffer register */
	short	tutcs;	/* transmitter status register */
	short	tutdb;	/* transmitter data buffer register */
};

/*
 * Receiver status register status/command bits
 */
#define TURCS_DONE	0x80	/* Receiver done (byte ready) */
#define TURCS_INTR	0x40	/* Receiver interrupt enable */

/*
 * Receiver data buffer register status bits
 */
#define	TURDB_ERROR	0x8000	/* Error (overrun or break) */
#define TURDB_ORUN	0x4000	/* Data overrun error */
#define	TURDB_BREAK	0x2000	/* TU58 break */

/*
 * Transmitter status register status/command bits
 */
#define	TUTCS_READY	0x80	/* transmitter ready */
#define	TUTCS_INTR	0x40	/* transmitter interrupt enable */
#define	TUTCS_MAINT	0x02	/* maintenance check */
#define	TUTCS_BREAK	0x01	/* send break */

#define	TUDB_DMASK	0x00ff	/* data mask (send and receive data) */

