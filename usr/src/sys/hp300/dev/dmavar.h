/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dmavar.h	7.1 (Berkeley) %G%
 */

/* dmago flags */
#define DMAGO_BYTE	0x00
#define DMAGO_WORD	0x01
#define DMAGO_LWORD	0x02
#define	DMAGO_PRI	0x04
#define	DMAGO_READ	0x08
#define	DMAGO_NOINT	0x80

/* dma "controllers" (channels) */
#define	DMA0		0x1
#define	DMA1		0x2

#ifdef KERNEL
extern void	dmago(), dmafree();
extern int	dmareq();
#endif
