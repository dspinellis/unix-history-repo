
/*	uureg.h	4.3	83/05/08	*/

/*
 * DL11-E/DL11-W UNIBUS (for TU58) controller registers
 */
struct uudevice {
	short	uurcs;	/* receiver status register */
	short	uurdb;	/* receiver data buffer register */
	short	uutcs;	/* transmitter status register */
	short	uutdb;	/* transmitter data buffer register */
};

/*
 * Receiver status register status/command bits
 */
#define UURCS_DONE	0x80	/* Receiver done (byte ready) */
#define UURCS_INTR	0x40	/* Receiver interrupt enable */

/*
 * Receiver data buffer register status bits
 */
#define	UURDB_ERROR	0x8000	/* Error (overrun or break) */
#define UURDB_ORUN	0x4000	/* Data overrun error */
#define	UURDB_BREAK	0x2000	/* TU58 break */

/*
 * Transmitter status register status/command bits
 */
#define	UUTCS_READY	0x80	/* transmitter ready */
#define	UUTCS_INTR	0x40	/* transmitter interrupt enable */
#define	UUTCS_MAINT	0x02	/* maintenance check */
#define	UUTCS_BREAK	0x01	/* send break */

#define	UUDB_DMASK	0x00ff	/* data mask (send and receive data) */

