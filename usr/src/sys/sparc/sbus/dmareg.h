/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dmareg.h	7.1 (Berkeley) %G%
 *
 * from: $Header: dmareg.h,v 1.4 92/06/17 06:59:33 torek Exp $ (LBL)
 */

/*
 * Sun-4c Sbus slot 0 DMA registers.
 */
struct dmareg {
	u_long	dma_csr;	/* control/status register */
	u_long	dma_addr;	/* address (virtual: is fed to MMU) */
	u_long	dma_bc;		/* byte count (not used) */
	u_long	dma_diag;	/* diagnostic register (not used) */
};

/*
 * Bits in dma_csr.
 *
 * Notes in [brackets]:
 *	1: not self-clearing, must be reset after being set.
 *	2: `drain' is like Unibus `bdp purge', i.e., it tells
 *	   the chip to finish up, because there is no more data
 *	   going into the buffer register.  Needed only in rev
 *	   1 dma chips.  Self-clearing (hence write-only).
 *	3: only in rev 1 dma chips.
 *	4: only in rev 2 dma chips.
 *	5: also enables scsi interrupts.
 */
#define	DMA_REV(csr)	(((csr) >> 28) & 0xf)	/* device id field */
#define	DMAREV_1	0x8		/* device id = rev 1 DMA */
#define	DMAREV_2	0x9		/* device id = rev 2 DMA */

#define	DMA_1ZERO	0x0fff0000	/* unused; reads as zero [3] */
#define	DMA_NAL		0x08000000	/* next address loaded [4] (ro) */
#define	DMA_AL		0x04000000	/* address loaded [4] (ro) */
#define	DMA_ON		0x02000000	/* working [4] (ro) */
#define	DMA_NAE		0x01000000	/* next-address enable [4] (rw) */
#define	DMA_DTCI	0x00800000	/* disable DMA_TC intr [4] (rw) */
#define	DMA_TURBO	0x00400000	/* faster 53C90A mode [4] (rw) */
#define	DMA_LERR	0x00200000	/* LANCE error [4] (ro) */
#define	DMA_ALE		0x00100000	/* LANCE addr latch ena [4] (rw) */
#define	DMA_2ZERO	0x000f0000	/* unused; reads as zero [4] */
#define	DMA_ILACC	0x00008000	/* set for new AMD ethernet chip */
#define	DMA_TC		0x00004000	/* terminal count: dma_bc ran out */
#define	DMA_BCE		0x00002000	/* byte count enable (leave 0) */
#define	DMA_BO		0x00001800	/* byte offset (ro) */
#define	DMA_RP		0x00000400	/* request pending (ro) */
#define	DMA_ENA		0x00000200	/* enable the dma chip */
#define	DMA_READ	0x00000100	/* set for dev=>mem, i.e., read() */
#define	DMA_RESET	0x00000080	/* reset dma chip [1] */
#define	DMA_DRAIN	0x00000040	/* drain buffered data [2,3] (wo) */
#define	DMA_ERR		0x00000040	/* slave error [4] (ro) */
#define	DMA_FLUSH	0x00000020	/* clear PC, EP, and TC [4] (wo) */
#define	DMA_IE		0x00000010	/* interrupt enable [4,5] */
#define	DMA_PC		0x0000000c	/* bytes in pack reg (ro) */
#define	DMA_EP		0x00000002	/* error pending (ro) */
#define	DMA_IP		0x00000001	/* interrupt pending (ro) */

#define	DMA_BITS \
"\20\34NAL\33AL\32ON\31NAE\30DTCI\27TURBO\26LERR\25ALE\
\20ILACC\17TC\16BCE\13RP\12ENA\11READ\10RESET\7DRAIN/ERR\6FLUSH\5IE\2EP\1IP"

/* DMA_BYTE turns the DMA_BO field into a byte index */
#define	DMA_BYTE(csr)	(((csr) >> 11) & 3)

/* DMA_NPACK turns the DMA_PC field into a byte count */
#define	DMA_NPACK(csr)	(((csr) >> 2) & 3)

/* DMA_INTR is true if the DMA chip says an ESP or DMA interrupt is pending */
#define	DMA_INTR(csr)	((csr) & (DMA_IP | DMA_EP))
