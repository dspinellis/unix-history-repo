/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dmavar.h	7.1 (Berkeley) 5/8/90
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
