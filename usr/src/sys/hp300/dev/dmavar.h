/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dmavar.h	7.2 (Berkeley) %G%
 */

/* dmago flags */
#define	DMAGO_BYTE	0x00	/* do byte (8 bit) transfers */
#define	DMAGO_WORD	0x01	/* do word (16 bit) transfers */
#define	DMAGO_LWORD	0x02	/* do longword (32 bit) transfers */
#define	DMAGO_PRI	0x04	/* do "priority" DMA */
#define	DMAGO_READ	0x08	/* transfer is a read */
#define	DMAGO_NOINT	0x80	/* don't interrupt on completion */

/* dma "controllers" (channels) */
#define	DMA0		0x1
#define	DMA1		0x2

#ifdef KERNEL
extern void	dmago(), dmafree();
extern int	dmareq();
#endif
