/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmfreg.h	7.2 (Berkeley) %G%
 */

/*
 * DMF-32 definitions.
 */

/*
 * "dmfa" refers to the async portion of the dmf32,
 * "dmfc" to the combo portion,
 * "dmfs" to the sync portion,
 * "dmfl" to the lp portion, and
 * "dmfd" to the dr portion.
 */
struct dmfdevice {
	short	dmfccsr0;		/* combo csr 0 */
	short	dmfccsr1;		/* combo csr 1 */
	short	dmfs[4];		/* synch. device */
	struct	dmx_octet dmfa;		/* asynch portion */
	short	dmfl_ctrl;		/* line printer control register */
	short	dmfl_indrct;		/* line printer indirect register */
	short	dmfd[4];		/* for dr11 (not implemented) */
};


/* dmf line printer csr def */
#define DMFL_PEN	(1<<0)		/* print enable */
#define DMFL_RESET	(1<<1)		/* master reset */
#define DMFL_FORMAT	(1<<2)		/* format control */
#define DMFL_UNUSED	(3<<3)
#define DMFL_MAINT	(1<<5)		/* maintenance mode on */
#define DMFL_IE		(1<<6)		/* intr enable */
#define DMFL_PDONE	(1<<7)		/* print done bit */
#define DMFL_INDIR	(7<<8)		/* indirect reg */
#define DMFL_UNUSED2	(1<<11)
#define DMFL_CONV	(1<<12)		/* connect verify */
#define	DMFL_DAVRDY	(1<<13)		/* davfu ready */
#define DMFL_OFFLINE	(1<<14)		/* printer offline */
#define DMFL_DMAERR	(1<<15)		/* dma error bit */
#define DMFL_BUFSIZ	512		/* max chars per dma */
#define DMFL_DEFCOLS	132		/* default # of cols/line <=255 */
#define DMFL_DEFLINES	66		/* default # of lines/page <=255 */
#define DMFL_OPTIONS	((1 << 8) | (1 << 9) | (1 << 15))
				/* auto cr, real ff, no lower to upper */

/*
 *  Bits in the configuration register
 */
#define DMFC_CONFMASK	0xf000		/* picks off the configuration bits */
#define	DMFC_DR		0x1000		/* DR11 parallel interface */
#define DMFC_LP		0x2000		/* LP dma parallel lineprinter i'face */
#define DMFC_SYNC	0x4000		/* Synchronous serial interface */
#define DMFC_ASYNC	0x8000		/* 8 Serial ports */
