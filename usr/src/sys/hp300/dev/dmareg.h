/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)dmareg.h	8.1 (Berkeley) 6/10/93
 */

#include <hp/dev/iotypes.h>		/* XXX */

/*
 * Hardware layout for the 98620[ABC]:
 *	98620A (old 320s?):	byte/word DMA in up to 64K chunks
 *	98620B (320s only):	98620A with programmable IPL
 *	98620C (all others):	byte/word/longword DMA in up to 4Gb chunks
 */

struct	dmaBdevice {
	v_char		*dmaB_addr;
	vu_short	dmaB_count;
	vu_short	dmaB_cmd;
#define	dmaB_stat	dmaB_cmd
};

struct	dmadevice {
	v_char		*dma_addr;
	vu_int		dma_count;
	vu_short	dma_cmd;
	vu_short	dma_stat;
};

struct	dmareg {
	struct dmaBdevice dma_Bchan0;
	struct dmaBdevice dma_Bchan1;
/* the rest are 98620C specific */
	v_char		  dma_id[4];
	vu_char		  dma_cr;
	char		  dma_pad1[0xEB];
	struct dmadevice  dma_chan0;
	char		  dma_pad2[0xF4];
	struct dmadevice  dma_chan1;
};

#define	NDMA		2

/* intr level must be >= level of any device using dma.  i.e., splbio */
#define	DMAINTLVL	5

/* addresses */
#define	DMA_BASE	IIOV(0x500000)

/* command bits */
#define	DMA_ENAB	0x0001
#define	DMA_WORD	0x0002
#define	DMA_WRT		0x0004
#define	DMA_PRI		0x0008
#define	DMA_IPL(x)	(((x) - 3) << 4)
#define DMA_LWORD	0x0100
#define DMA_START	0x8000

/* status bits */
#define	DMA_ARMED	0x01
#define	DMA_INTR	0x02
#define DMA_ACC		0x04
#define DMA_HALT	0x08
#define DMA_BERR	0x10
#define DMA_ALIGN	0x20
#define DMA_WRAP	0x40

#ifdef KERNEL
/*
 * Macros to attempt to hide the HW differences between the 98620B DMA
 * board and the 1TQ4-0401 DMA chip (68020C "board").  The latter
 * includes emulation registers for the former but you need to access
 * the "native-mode" registers directly in order to do 32-bit DMA.
 *
 * DMA_CLEAR:	Clear interrupt on DMA board.  We just use the
 *		emulation registers on the 98620C as that is easiest.
 * DMA_STAT:	Read status register.  Again, we always read the
 *		emulation register.  Someday we might want to
 *		look at the 98620C status to get the extended bits.
 * DMA_ARM:	Load address, count and kick-off DMA.
 */
#define	DMA_CLEAR(dc)	{ v_int dmaclr = (int)dc->sc_Bhwaddr->dmaB_addr; }
#define	DMA_STAT(dc)	dc->sc_Bhwaddr->dmaB_stat

#if defined(HP320)
#define	DMA_ARM(dc)	\
	if (dc->sc_type == DMA_B) { \
		register struct dmaBdevice *dma = dc->sc_Bhwaddr; \
		dma->dmaB_addr = dc->sc_cur->dc_addr; \
		dma->dmaB_count = dc->sc_cur->dc_count - 1; \
		dma->dmaB_cmd = dc->sc_cmd; \
	} else { \
		register struct dmadevice *dma = dc->sc_hwaddr; \
		dma->dma_addr = dc->sc_cur->dc_addr; \
		dma->dma_count = dc->sc_cur->dc_count - 1; \
		dma->dma_cmd = dc->sc_cmd; \
	}
#else
#define	DMA_ARM(dc)	\
	{ \
		register struct dmadevice *dma = dc->sc_hwaddr; \
		dma->dma_addr = dc->sc_cur->dc_addr; \
		dma->dma_count = dc->sc_cur->dc_count - 1; \
		dma->dma_cmd = dc->sc_cmd; \
	}
#endif
#endif
