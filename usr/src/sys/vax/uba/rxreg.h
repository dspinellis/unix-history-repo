/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rxreg.h	7.2 (Berkeley) %G%
 */

#ifdef KERNEL
#include "ioctl.h"
#else
#include <sys/ioctl.h>
#endif

/*
 * RX02 registers
 */
struct rxdevice {
	short	rxcs;		/* control/status register */
	short	rxdb;		/* data buffer register */
};

/*
 * RX211 Command and Status Register (RX2CS)
 */
#define	RX_DRV0		0x0000	/* select drive 0 */
#define	RX_DRV1		0x0010	/* select drive 1 */
#define	RX_DONE		0x0020	/* function complete */
#define	RX_INTR		0x0040	/* interrupt enable */
#define	RX_TREQ		0x0080	/* transfer request (data only)	*/
#define	RX_SDEN		0x0000	/* single density */
#define	RX_DDEN		0x0100	/* double density */
#define	RX_EXT		0x3000	/* extended address bits */
#define	RX_INIT		0x4000	/* initialize RX211 interface */
#define	RX_ERR		0x8000	/* general error bit */

/*
 * RX211 control function bits (0-3 of RX2CS)
 */
#define	RX_FILL		0x0001	/* fill the buffer */
#define	RX_EMPTY	0x0003	/* empty the buffer */
#define	RX_WRITE	0x0005	/* write the buffer to disk */
#define	RX_READ		0x0007	/* read a disk sector to the buffer */
#define	RX_FORMAT	0x0009	/* set the media density (format) */
#define	RX_RDSTAT	0x000b	/* read the disk status */
#define	RX_WDDS		0x000d	/* write a deleted-data sector */
#define	RX_RDERR	0x000f	/* read the error registers */

#define	RXCS_BITS \
"\20\20RX_ERR\17RX_INIT\11RX_DDEN\10RX_TREQ\7RX_IE\6RX_DONE\5RX_DRV1"

/*
 * RX211 Error and Status Register (RX2ES) --
 * information is located in RX2DB after completion of function.
 * The READY bit's value is available only after a "read status".
 */
#define	RXES_CRCERR	0x0001	/* CRC error (data read error) */
#define	RXES_IDONE	0x0004	/* reinitialization complete */
#define RXES_DENERR	0x0010	/* density error */
#define	RXES_DBLDEN	0x0020	/* set if double density */
#define	RXES_DDMARK	0x0040	/* deleted-data mark */
#define	RXES_READY	0x0080	/* drive is ready */

#define	RXES_BITS \
"\20\14RXES_NXM\13RXES_WCOF\11RXES_DRV1\10RXES_RDY\7RXES_DDMK\6RXES_DDEN\5\
RXES_DNER\4RXES_ACLO\3RXES_ID\1RXES_CRC"

/* 
 * Ioctl commands, move to dkio.h later
 */
#define RXIOC_FORMAT	_IOW('d', 1, int)	/* format the disk */
#define RXIOC_WDDS	_IOW('d', 2, int)	/* write `deleted data' mark */
						/* on next sector */
#define RXIOC_RDDSMK	_IOR('d', 3, int)	/* did last read sector */
						/* contain `deleted data'?*/
#define	RXIOC_GDENS	_IOR('d', 4, int)	/* return density of current */
						/* disk */

#ifdef RXDEFERR
/*
 * Table of values for definitive error code (rxxt[0] & 0xff)
 */
struct rxdeferr {
	short	errval;
	char	*errmsg;
} rxdeferr[] = {
	{ 0010,	"Can't find home on drive 0" },
	{ 0020,	"Can't find home on drive 1" },
	{ 0040,	"Bad track number requested" },
	{ 0050,	"Home found too soon" },
	{ 0070,	"Can't find desired sector" },
	{ 0110,	"No SEP clock seen" },
	{ 0120,	"No preamble found" },
	{ 0130,	"Preamble, but no ID mark" },
	{ 0140, "Header CRC error"},
	{ 0150,	"Track addr wrong in header" },
	{ 0160,	"Too many tries for ID AM" },
	{ 0170,	"No data AM found" },
	{ 0200,	"Data CRC error" },
	{ 0220,	"Maintenance test failure" },
	{ 0230,	"Word count overflow" },
	{ 0240,	"Density error" },
	{ 0250,	"Set-density protocol bad" },
	{ 0,	"Undefined error code" }
};
#endif
