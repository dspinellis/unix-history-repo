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
 *	@(#)dma.c	7.1 (Berkeley) 5/8/90
 */

/*
 * DMA driver
 */

#include "param.h"
#include "systm.h"
#include "time.h"
#include "kernel.h"
#include "proc.h"
#include "dmareg.h"
#include "dmavar.h"
#include "device.h"

#include "machine/cpu.h"
#include "machine/isr.h"

extern void isrlink();
extern void printf();
extern void panic();
extern void _insque();
extern void _remque();
extern void timeout();
extern int splbio();
extern void splx();
extern u_int kvtop();
extern void PCIA();

/*
 * The largest single request will be MAXPHYS bytes which will require
 * at most MAXPHYS/NBPG+1 chain elements to describe, i.e. if none of
 * the buffer pages are physically contiguous (MAXPHYS/NBPG) and the
 * buffer is not page aligned (+1).
 */
#define	DMAMAXIO	(MAXPHYS/NBPG+1)

#define	DMATIMO		15

struct	dma_softc {
	struct dmadevice *sc_hwaddr;
	struct dmaBdevice *sc_Bhwaddr;
	int	sc_type;
	int	sc_cur;
	int	sc_cmd;
	int	sc_timo;
	int	sc_count[DMAMAXIO+1];
	char	*sc_addr[DMAMAXIO+1];
} dma_softc[NDMA];

/* types */
#define	DMA_B	0
#define DMA_C	1

struct	devqueue dmachan[NDMA + 1];
int	dmaintr();
void	dmatimo();

#ifdef DEBUG
int	dmadebug = 0;
#define DDB_WORD	0x01	/* same as DMAGO_WORD */
#define DDB_LWORD	0x02	/* same as DMAGO_LWORD */
#define	DDB_FOLLOW	0x04
#define DDB_IO		0x08

long	dmahits[NDMA];
long	dmamisses[NDMA];
long	dmabyte[NDMA];
long	dmaword[NDMA];
long	dmalword[NDMA];
#endif

void
dmainit()
{
	register struct dmareg *dma = (struct dmareg *)DMA_BASE;
	register struct dma_softc *dc;
	register int i;
	char rev;

	/*
	 * Determine the DMA type.
	 * Don't know how to easily differentiate the A and B cards,
	 * so we just hope nobody has an A card (A cards will work if
	 * DMAINTLVL is set to 3).
	 */
	if (!badbaddr((char *)&dma->dma_id[2]))
		rev = dma->dma_id[2];
	else {
		rev = 'B';
#if !defined(HP320)
		panic("dmainit: DMA card requires hp320 support");
#endif
	}

	dc = &dma_softc[0];
	for (i = 0; i < NDMA; i++) {
		dc->sc_hwaddr = (i & 1) ? &dma->dma_chan1 : &dma->dma_chan0;
		dc->sc_Bhwaddr = (i & 1) ? &dma->dma_Bchan1 : &dma->dma_Bchan0;
		dc->sc_type = rev == 'B' ? DMA_B : DMA_C;
		dc++;
		dmachan[i].dq_forw = dmachan[i].dq_back = &dmachan[i];
	}
	dmachan[i].dq_forw = dmachan[i].dq_back = &dmachan[i];
	timeout(dmatimo, (caddr_t)0, DMATIMO * hz);

	printf("dma: 98620%c with 2 channels, %d bit DMA\n",
	       rev, rev == 'B' ? 16 : 32);
}

int
dmareq(dq)
	register struct devqueue *dq;
{
	register int i;
	register int chan;
	register int s = splbio();

	chan = dq->dq_ctlr;
	i = NDMA;
	while (--i >= 0) {
		if ((chan & (1 << i)) == 0)
			continue;
		if (dmachan[i].dq_forw != &dmachan[i])
			continue;
		insque(dq, &dmachan[i]);
		dq->dq_ctlr = i;
		splx(s);
		return(1);
	}
	insque(dq, dmachan[NDMA].dq_back);
	splx(s);
	return(0);
}

void
dmafree(dq)
	register struct devqueue *dq;
{
	int unit = dq->dq_ctlr;
	register struct dma_softc *dc = &dma_softc[unit];
	register struct devqueue *dn;
	register int chan, s;

	s = splbio();
	dc->sc_timo = 0;
	DMA_CLEAR(dc);
	remque(dq);
	chan = 1 << unit;
	for (dn = dmachan[NDMA].dq_forw;
	     dn != &dmachan[NDMA]; dn = dn->dq_forw) {
		if (dn->dq_ctlr & chan) {
			remque((caddr_t)dn);
			insque((caddr_t)dn, (caddr_t)dq->dq_back);
			splx(s);
			dn->dq_ctlr = dq->dq_ctlr;
			(dn->dq_driver->d_start)(dn->dq_unit);
			return;
		}
	}
	splx(s);
}

void
dmago(unit, addr, count, flags)
	int unit;
	register char *addr;
	register int count;
	register int flags;
{
	register struct dma_softc *dc = &dma_softc[unit];
	register char *dmaend = NULL;
	register int tcount, i;

#ifdef DEBUG
	if (dmadebug & DDB_FOLLOW)
		printf("dmago(%d, %x, %x, %x)\n",
		       unit, addr, count, flags);
	if (flags & DMAGO_LWORD)
		dmalword[unit]++;
	else if (flags & DMAGO_WORD)
		dmaword[unit]++;
	else
		dmabyte[unit]++;
#endif
#if defined(HP320)
	if (dc->sc_type == DMA_B && (flags & DMAGO_LWORD))
		panic("dmago: no can do 32-bit DMA");
#endif
	/*
	 * Build the DMA chain
	 */
	for (i = 0; i < DMAMAXIO && count; i++) {
		dc->sc_addr[i] = (char *)kvtop(addr);
		tcount = dc->sc_count[i] =
			MIN(count, NBPG - ((int)addr & PGOFSET));
		addr += dc->sc_count[i];
		count -= tcount;
		if (flags & (DMAGO_WORD|DMAGO_LWORD))
			tcount >>= (flags & DMAGO_WORD) ? 1 : 2;
		if (dc->sc_addr[i] == dmaend
#if defined(HP320)
		    /* only 16-bit count on 98620B */
		    && (dc->sc_type != DMA_B ||
			dc->sc_count[i-1] + tcount <= 65536)
#endif
		) {
#ifdef DEBUG
			dmahits[unit]++;
#endif
			dmaend += dc->sc_count[i];
			dc->sc_count[i-1] += tcount;
			i--;
		} else {
#ifdef DEBUG
			dmamisses[unit]++;
#endif
			dmaend = dc->sc_addr[i] + dc->sc_count[i];
			dc->sc_count[i] = tcount;
		}
	}
	if (count)
		panic("dmago maxphys");
	dc->sc_count[i] = 0;
	dc->sc_cur = 0;
	/*
	 * Set up the command word based on flags
	 */
	dc->sc_cmd = DMA_ENAB | DMA_IPL(DMAINTLVL) | DMA_START;
	if ((flags & DMAGO_READ) == 0)
		dc->sc_cmd |= DMA_WRT;
	if (flags & DMAGO_LWORD)
		dc->sc_cmd |= DMA_LWORD;
	else if (flags & DMAGO_WORD)
		dc->sc_cmd |= DMA_WORD;
	if (flags & DMAGO_PRI)
		dc->sc_cmd |= DMA_PRI;

	/*
	 * We should be able to skip the dma completion interrupt
	 * if we only have one segment in the chain since many
	 * devices generate their own completion interrupt.
	 * However, on a 370 we have to take the interrupt on
	 * read transfers to invalidate the external cache.
	 */
	if ((flags & DMAGO_NOINT) && i == 1
#if defined(HP370)
	    && ((flags & DMAGO_READ) == 0 || ectype != EC_PHYS)
#endif
	)
		dc->sc_cmd &= ~DMA_ENAB;
#ifdef DEBUG
#if defined(HP320)
	/* would this hurt? */
	if (dc->sc_type == DMA_B)
		dc->sc_cmd &= ~DMA_START;
#endif
	if (dmadebug & DDB_IO)
		if ((dmadebug&DDB_WORD) && (dc->sc_cmd&DMA_WORD) ||
		    (dmadebug&DDB_LWORD) && (dc->sc_cmd&DMA_LWORD)) {
			printf("dmago: cmd %x\n", dc->sc_cmd);
			for (i = 0; dc->sc_count[i]; i++) 
				printf("  %d: %d@%x\n",
				       i, dc->sc_count[i], dc->sc_addr[i]);
		}
#endif

	/*
	 * Load and arm the channel
	 */
	dc->sc_timo = 1;
	DMA_ARM(dc, 0);
}

void
dmastop(unit)
	register int unit;
{
	register struct dma_softc *dc = &dma_softc[unit];
	register struct devqueue *dq;

#ifdef DEBUG
	if (dmadebug & DDB_FOLLOW)
		printf("dmastop(%d)\n", unit);
#endif
	dc->sc_timo = 0;
	DMA_CLEAR(dc);

	/*
	 * We may get this interrupt after a device service routine
	 * has freed the dma channel.  So, ignore the intr if there's
	 * nothing on the queue.
	 */
	dq = dmachan[unit].dq_forw;
	if (dq != &dmachan[unit]) {
#if defined(HP370)
		/*
		 * The 370 has an 64k external physical address cache.
		 * In theory, we should only need to flush it when
		 * DMAing to memory.
		 */
		if (ectype == EC_PHYS && (dc->sc_cmd & DMA_WRT) == 0)
			PCIA();
#endif
		(dq->dq_driver->d_done)(dq->dq_unit);
	}
}

int
dmaintr()
{
	register struct dma_softc *dc;
	register int i, j, stat;
	int found = 0;

#ifdef DEBUG
	if (dmadebug & DDB_FOLLOW)
		printf("dmaintr\n");
#endif
	for (i = 0, dc = dma_softc; i < NDMA; i++, dc++) {
		stat = DMA_STAT(dc);
		if ((stat & DMA_INTR) == 0)
			continue;
		found++;
#ifdef DEBUG
		if (dmadebug & DDB_IO) {
			if ((dmadebug&DDB_WORD) && (dc->sc_cmd&DMA_WORD) ||
			    (dmadebug&DDB_LWORD) && (dc->sc_cmd&DMA_LWORD))
				printf("dmaintr: unit %d stat %x next %d\n",
				       i, stat, dc->sc_cur+1);
		}
		if (stat & DMA_ARMED)
			printf("dma%d: intr when armed\n", i);
#endif
		j = ++dc->sc_cur;
		if (j < DMAMAXIO && dc->sc_count[j]) {
			dc->sc_timo = 1;
			DMA_CLEAR(dc);
			DMA_ARM(dc, j);
		} else
			dmastop(i);
	}
	return(found);
}

void
dmatimo()
{
	register int i, s;
	register struct dma_softc *dc = &dma_softc[0];

	for (i = 0; i < NDMA; i++, dc++) {
		s = splbio();
		if (dc->sc_timo) {
			if (dc->sc_timo == 1)
				dc->sc_timo++;
			else
				dmastop(i);
		}
		splx(s);
	}
	timeout(dmatimo, (caddr_t)0, DMATIMO * hz);
}
