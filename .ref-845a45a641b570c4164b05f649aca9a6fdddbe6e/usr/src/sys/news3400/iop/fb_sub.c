/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: fb_sub.c,v 4.300 91/06/27 20:43:09 root Rel41 $ SONY
 *
 *	@(#)fb_sub.c	7.3 (Berkeley) %G%
 */

#include "fb.h"
#if NFB > 0
/*
 * Frame buffer driver
 */

#include <sys/types.h>
#include <machine/pte.h>
#include <machine/cpu.h>
#include <machine/param.h>

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/buf.h>
#include <vm/vm.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/uio.h>
#include <sys/kernel.h>

#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>
#ifdef CPU_DOUBLE
#ifdef IPC_MRX
#include "../ipc/newsipc.h"
#ifdef mips
#define ipc_phys(x)	K0_TT0(x)
#define ipc_log(x)	TT0_K0(x)
#else /* mips */
#define	ipc_phys(x)	(caddr_t)((int)(x) & ~0x80000000)
#define	ipc_log(x)	(caddr_t)((int)(x) | 0x80000000)
#endif /* mips */
#endif /* IPC_MRX */
#else /* CPU_DOUBLE */
#define ipc_phys(x)	(caddr_t)((int)(x))
#define ipc_log(x)	(caddr_t)((int)(x) | 0x80000000)
#endif /* CPU_DOUBLE */

#define MAX_FBMAP	3
static struct fb_map	fbmap[MAX_FBMAP];

/* bitblt type */
#define BLTTYPE(ts, td)	((ts) << 2 | (td))

static lSrcDest	srcdestlist[MAX_BATCHBITBLT];
#define MAX_SIZE	(MAX_BATCHBITBLT * sizeof(lSrcDest))

#define ODD_ADDR(x)	(((unsigned)(x))&1)

#define MAXMSEG	4
static int	segind;
static struct memseg {
	caddr_t		adrs;
	int		len;
	int		rw;
	caddr_t		oadrs;
	int		olen;
	struct fb_map	*map;
} mseg[MAXMSEG];

#define BASE(a)	((int)(a) & ~(NBPG - 1))

#ifdef CPU_DOUBLE
COPYIN(src, dst, len, seg)
	caddr_t src;
	caddr_t dst;
	int len;
	int seg;
{

	switch (seg) {
	case UIO_SYSSPACE:
		bcopy(src, dst, len);
		return (0);
	case UIO_USERSPACE:
		return (copyin(src, dst, len));
	default:
		panic("COPYIN: seg");
		/* NOTREACHED */
	}
}
#endif /* CPU_DOUBLE */

fbgetmap(addr, len, map)
	caddr_t addr;
	int len;
	struct fb_map *map;
{
	register struct pte *pte;
	register caddr_t *p;
	unsigned v;
	register int npf;
	int o;

	v = pmax_btop(addr);
	o = (int)addr & PGOFSET;
	npf = btoc(len + o);
	if (npf > NFBMAP)
		return (EINVAL);

	map->fm_vaddr = addr;
	map->fm_offset = o;
	map->fm_count = len;

#ifdef CPU_DOUBLE
	if (addr >= (caddr_t)KERNBASE) {
#ifdef mips
		p = map->fm_addr;
		if (MACH_IS_CACHED(addr)) {
			addr = ptob(v);
			while (--npf >= 0) {
				*p++ = (caddr_t)K0_TT0(addr);
				addr += NBPG;
			}
			return (0);
		}
		if (MACH_IS_MAPPED(addr))
			pte = kvtopte(addr);
		else if (addr >= (caddr_t)&u)
			pte = curproc->p_addr + btop(addr - (caddr_t)&u);
		else
			panic("fbgetmap: bad kernel addr");
		while (--npf >= 0)
			*p++ = (caddr_t)PHYS_TT0(ptob(pte++->pg_pfnum));
#else /* mips */
		pte = kvtopte(addr);
		p = map->fm_addr;
		while (--npf >= 0)
			*p++ = (caddr_t)ptob(pte++->pg_pfnum);
#endif /* mips */
		return (0);
	}
	pte = vtopte(curproc, v);		/*KU:XXXXXXXXXXXXXXXXXX*/
	p = map->fm_addr;
	while (--npf >= 0) {
		if (pte->pg_pfnum == 0)
			panic("iop zero uentry");
#ifdef mips
		*p++ = (caddr_t)PHYS_TT0(ptob(pte++->pg_pfnum));
#else
		*p++ = (caddr_t)ptob(pte++->pg_pfnum);
#endif
	}

#endif /* CPU_DOUBLE */
	return (0);
}

fblockmem(ad, len, rw, map, seg)
	register caddr_t ad;
	register int len;
	int rw;
	struct fb_map *map;
	int seg;
{
	register struct memseg *msp;

	/* validity check */
	if (len < 0)
		return EFAULT;
	else if (len == 0)
		return 0;
	else if (ad == 0)
		return EFAULT;
	else if (seg == UIO_USERSPACE &&
		 !useracc(ad, len, rw == B_READ ? B_WRITE : B_READ))
		return EFAULT;
	else if (segind >= MAXMSEG)
		return EFAULT;

	/* insertion sort */
	for (msp = mseg + segind - 1; msp >= mseg; msp--) {
		if (msp->adrs > ad)
			*(msp + 1) = *msp;
		else
			break;
	}
	msp++;
#ifdef CPU_SINGLE
	switch (seg) {
	case UIO_SYSSPACE:
		map->fm_vaddr = ad;
		map->fm_offset = 0;
		break;
	case UIO_USERSPACE:
		msp->adrs = (caddr_t)BASE(ad);
		msp->len = (caddr_t)BASE(ad + len + NBPG - 1) - msp->adrs;
		break;
	default:
		panic("fblockmem: seg");
		/* NOTREACHED */
	}
#else /* CPU_SINGLE */
	msp->adrs = (caddr_t)BASE(ad);
	msp->len = (caddr_t)BASE(ad + len + NBPG - 1) - msp->adrs;
#endif /* CPU_SINGLE */
	msp->rw = rw;
	msp->oadrs = ad;
	msp->olen = len;
	msp->map = map;
	segind++;

	return (0);
}

fblocksbitmap(bm, rw, map)
	register lBitmap *bm;
	int rw;
	struct fb_map *map;
{
	register int len;
	register int error = 0;

	if (bm->depth > 32)
		return EINVAL;
	if (bm->type == BM_FB || bm->type == BM_0 || bm->type == BM_1)
		return 0;
	if (bm->type == BM_MEM)
		len = bm->width * bm->rect.extent.y * bm->depth * sizeof(Word);
	if (len < 0 || len > FB_MAX_IO)
		return(EINVAL);
	error = fblockmem((caddr_t)(bm->base), len, rw, map, UIO_USERSPACE);
	if (error)
		return error;

	bm->base = (Word *)ipc_phys(map);
	return 0;
}

void
fbinitlock()
{
	segind = 0;
}

void
fbdolock()
{
	register struct memseg *msp0, *msp1, *mspl;
	register int i, tlen;

	mspl = mseg + segind;
	for (msp0 = mseg, msp1 = mseg + 1; msp1 < mspl; msp0++, msp1++) {
		if (msp0->adrs + msp0->len > msp1->adrs) {
			tlen = msp1->adrs - msp0->adrs + msp1->len;
			msp1->adrs = msp0->adrs;
			msp1->len = msp0->len > tlen ? msp0->len : tlen;
			msp0->len = 0;
			msp1->rw = (msp0->rw == B_READ || msp1->rw == B_READ) ?
					B_READ : B_WRITE;
		}
	}

	/* lock */
	curproc->p_flag |= SPHYSIO;
	for (i = 0; i < segind; i++)
		if (mseg[i].len && mseg[i].adrs && mseg[i].adrs
		    < (caddr_t)KERNBASE)
			vslock(mseg[i].adrs, mseg[i].len);

	/* make map */
	for (i = 0; i < segind; i++)
		fbgetmap(mseg[i].oadrs, mseg[i].olen, mseg[i].map);
}

void
fbunlock()
{
	register int i;
	int s = splfb();

	for (i = 0; i < segind; i++) {
		if (mseg[i].len && mseg[i].adrs && mseg[i].adrs
		    < (caddr_t)KERNBASE) {
			vsunlock(mseg[i].adrs, mseg[i].len, mseg[i].rw);
#if defined(mips) && defined(CPU_DOUBLE)
			if (mseg[i].rw == B_READ)
				clean_kudcache(curproc,
					       mseg[i].adrs, mseg[i].len);
#endif
		}
	}
	curproc->p_flag &= ~SPHYSIO;
	splx(s);

	/* for 'fbinitlock() o wasureru ukkariyasan'... */
	segind = 0;
}

checkbitmap(bm)
	register lBitmap *bm;
{
	if (bm->depth > 32)
		return EINVAL;

	switch(bm->type) {
	case BM_FB:
	case BM_0:
	case BM_1:
		break;
	case BM_MEM:
		if (ODD_ADDR(bm->base))
			return EINVAL;
		if (bm->width == 0)
			return EINVAL;
		if ((bm->width * bm->rect.extent.y) << 1 > FB_MAX_IO)
			return EINVAL;
		break;
	default:
		return EINVAL;
	}

	if (bm->rect.extent.x < 0 || bm->rect.extent.y < 0)
		return EINVAL;
	else
		return 0;
}

checkdepth(sbm, dbm)
	lBitmap	*sbm, *dbm;
{
	register int ds = sbm->depth;
	register int dd = dbm->depth;

	if (ds > 1 && dd > 1 && ds != dd)
		return -1;
	else
		return((ds > 1) << 1 | (dd > 1));
}

dobitblt(fbp, sbm, dbm)
	struct fbreg *fbp;
	lBitmap	*sbm, *dbm;
{
	register int error;

	if (error = fblocksbitmap(sbm, B_WRITE, fbmap))
		return error;
	if (error = fblocksbitmap(dbm, B_READ, fbmap + 1))
		return error;
	fbdolock();
	fbstart(fbp, 1);
	fbunlock();

	/* reset address */
	if (sbm->type == BM_MEM)
		sbm->base =
		    (Word *)((struct fb_map *)ipc_log(sbm->base))->fm_vaddr;
	if (dbm->type == BM_MEM)
		dbm->base =
		    (Word *)((struct fb_map *)ipc_log(dbm->base))->fm_vaddr;
	return 0;
}

fbnbitblt(fbp, cmd)
	register struct fbreg *fbp;
	register lBitblt *cmd;
{
	register lBitblt *regcmd;
	register int len, lens, lend;
	register int i;
	int pmask, mode;
	int error = 0;
	int blttype = BLTTYPE(cmd->srcBitmap.type, cmd->destBitmap.type);

#ifdef CPU_DOUBLE
	if ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_1, BM_MEM))) {
		return(mfbnbitblt(fbp, cmd));
		/* NOTREACHED */
	}
#endif

	fbinitlock();
	if (error = checkbitmap(&cmd->srcBitmap))
		return error;
	if (error = checkbitmap(&cmd->destBitmap))
		return error;
	if ((mode = checkdepth(&cmd->srcBitmap, &cmd->destBitmap)) < 0)
		return EINVAL;

	fbp->fb_command = FB_CBITBLT;
	fbp->fb_bitblt = *cmd;
	regcmd = &fbp->fb_bitblt;

	/* process bitblt command */
	switch (blttype) {
	case BLTTYPE(BM_FB, BM_FB):
	case BLTTYPE(BM_0, BM_FB):
	case BLTTYPE(BM_1, BM_FB):
		fbstart(fbp, 0);
		break;

	case BLTTYPE(BM_FB, BM_MEM):
	case BLTTYPE(BM_0, BM_MEM):
	case BLTTYPE(BM_1, BM_MEM):
		len = cmd->destBitmap.width * cmd->destBitmap.rect.extent.y << 1;
		if (len * cmd->destBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		/* bitblt each plane */
		regcmd->destBitmap.depth = 1;
		pmask = regcmd->planemask;
		for (i = 0; i < cmd->destBitmap.depth; i++) {
			if (mode == 3)	/* N to N */
				regcmd->planemask = pmask & (1 << i);

			if (error = dobitblt(fbp, &regcmd->srcBitmap,
						  &regcmd->destBitmap))
				return error;

			regcmd->destBitmap.base += len >> 1;
			if (mode == 1) {	/* N to N */
				regcmd->planemask >>= 1;
				regcmd->fore_color >>= 1;
				regcmd->aux_color >>= 1;
			}
		}
		break;

	case BLTTYPE(BM_MEM, BM_FB):
		len = cmd->srcBitmap.width * cmd->srcBitmap.rect.extent.y << 1;

		if (len * cmd->srcBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		/* bitblt each plane */
		regcmd->srcBitmap.depth = 1;
		pmask = regcmd->planemask;
		regcmd->fore_color = 0xff;
		regcmd->aux_color = 0;
		if (mode == 2) {	/* N to 1 */
			for (i = 0; i < cmd->srcBitmap.depth; i++)
				if (pmask & (1 << i))
					break;
			if (i >= cmd->srcBitmap.depth)
				return 0;
			regcmd->srcBitmap.base += (len >> 1) * i;
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}
		/* else (N to N) */
		for (i = 0; i < cmd->srcBitmap.depth; i++) {
			regcmd->planemask = pmask & (1 << i);
			if (error = dobitblt(fbp, &regcmd->srcBitmap,
						  &regcmd->destBitmap))
				return error;
			regcmd->srcBitmap.base += len >> 1;
			regcmd->planemask >>= 1;
		}
		return 0;

	case BLTTYPE(BM_MEM, BM_MEM):
		lens = cmd->srcBitmap.width * cmd->srcBitmap.rect.extent.y << 1;
		lend = cmd->destBitmap.width * cmd->destBitmap.rect.extent.y << 1;
		if (lens * cmd->srcBitmap.depth <= FB_MAX_IO &&
		    lend * cmd->destBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		regcmd->srcBitmap.depth = 1;
		regcmd->destBitmap.depth = 1;
		pmask = regcmd->planemask;
		if (mode == 2) {	/* N to 1 */
			regcmd->fore_color = 0xff;
			regcmd->aux_color = 0;
			for (i = 0; i < cmd->srcBitmap.depth; i++)
				if (pmask & (1 << i))
					break;
			if (i >= cmd->srcBitmap.depth)
				return 0;
			regcmd->srcBitmap.base += (lens >> 1) * i;
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		} else if (mode == 1) {	/* 1 to N */
			for (i = 0; i < cmd->srcBitmap.depth; i++) {
				if (error = dobitblt(fbp, &regcmd->srcBitmap,
							  &regcmd->destBitmap))
					return error;
				regcmd->planemask >>= 1;
				regcmd->fore_color >>= 1;
				regcmd->aux_color >>= 1;
				regcmd->destBitmap.base += lend >> 1;
			}
			return 0;
		} else {		/* N to N */
			regcmd->fore_color = 0xff;
			regcmd->aux_color = 0;
			for (i = 0; i < cmd->srcBitmap.depth; i++) {
				if (error = dobitblt(fbp, &regcmd->srcBitmap,
							  &regcmd->destBitmap))
					return error;
				regcmd->srcBitmap.base += lens >> 1;
				regcmd->destBitmap.base += lend >> 1;
				regcmd->planemask >>= 1;
			}
			return 0;
		}
		break;

	default:
		return EINVAL;
	}

	return error;
}

fbbitblt(fbp, cmd)
	struct fbreg *fbp;
	register sBitblt *cmd;
{
	lBitblt lcmd;
	register lBitblt *lcmdp;

	lcmdp = &lcmd;

	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->srcBitmap.type = cmd->srcBitmap.type;
	lcmdp->srcBitmap.depth = cmd->srcBitmap.depth;
	lcmdp->srcBitmap.width = cmd->srcBitmap.width;
	lcmdp->srcBitmap.rect.origin.x = cmd->srcBitmap.rect.origin.x;
	lcmdp->srcBitmap.rect.origin.y = cmd->srcBitmap.rect.origin.y;
	lcmdp->srcBitmap.rect.extent.x = cmd->srcBitmap.rect.extent.x;
	lcmdp->srcBitmap.rect.extent.y = cmd->srcBitmap.rect.extent.y;
	lcmdp->srcBitmap.base = cmd->srcBitmap.base;
	lcmdp->srcRect.origin.x = cmd->srcRect.origin.x;
	lcmdp->srcRect.origin.y = cmd->srcRect.origin.y;
	lcmdp->srcRect.extent.x = cmd->srcRect.extent.x;
	lcmdp->srcRect.extent.y = cmd->srcRect.extent.y;
	lcmdp->destBitmap.type = cmd->destBitmap.type;
	lcmdp->destBitmap.depth = cmd->destBitmap.depth;
	lcmdp->destBitmap.width = cmd->destBitmap.width;
	lcmdp->destBitmap.rect.origin.x = cmd->destBitmap.rect.origin.x;
	lcmdp->destBitmap.rect.origin.y = cmd->destBitmap.rect.origin.y;
	lcmdp->destBitmap.rect.extent.x = cmd->destBitmap.rect.extent.x;
	lcmdp->destBitmap.rect.extent.y = cmd->destBitmap.rect.extent.y;
	lcmdp->destBitmap.base = cmd->destBitmap.base;
	lcmdp->destClip.origin.x = cmd->destClip.origin.x;
	lcmdp->destClip.origin.y = cmd->destClip.origin.y;
	lcmdp->destClip.extent.x = cmd->destClip.extent.x;
	lcmdp->destClip.extent.y = cmd->destClip.extent.y;
	lcmdp->destPoint.x = cmd->destPoint.x;
	lcmdp->destPoint.y = cmd->destPoint.y;
	return (fbnbitblt(fbp, lcmdp));
}

procbatchbitblt(fbp, mode, cmd)
	struct fbreg *fbp;
	int mode;
	register lBatchBitblt *cmd;
{
	register lBatchBitblt *regcmd;
	register int len, lens, lend;
	register int i;
	int pmask;
	int error = 0;

	regcmd = &fbp->fb_batchbitblt;
	/* process batch bitblt command */
	switch (BLTTYPE(regcmd->srcBitmap.type, regcmd->destBitmap.type)) {
	case BLTTYPE(BM_FB, BM_FB):
	case BLTTYPE(BM_0, BM_FB):
	case BLTTYPE(BM_1, BM_FB):
		fbdolock();
		fbstart(fbp, cmd->nSrcDest);
		fbunlock();
		break;

	case BLTTYPE(BM_FB, BM_MEM):
	case BLTTYPE(BM_0, BM_MEM):
	case BLTTYPE(BM_1, BM_MEM):
		len = cmd->destBitmap.width * cmd->destBitmap.rect.extent.y << 1;
		if (len * cmd->destBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		/* bitblt each plane */
		regcmd->destBitmap.depth = 1;
		pmask = regcmd->planemask;
		for (i = 0; i < cmd->destBitmap.depth; i++) {
			if (mode == 3)	/* N to N */
				regcmd->planemask = pmask & (1 << i);

			if (error = dobitblt(fbp, &regcmd->srcBitmap,
						  &regcmd->destBitmap))
				return error;

			regcmd->destBitmap.base += len >> 1;
			if (mode == 1) {	/* N to N */
				regcmd->planemask >>= 1;
				regcmd->fore_color >>= 1;
				regcmd->aux_color >>= 1;
			}
		}
		break;

	case BLTTYPE(BM_MEM, BM_FB):
		len = cmd->srcBitmap.width * cmd->srcBitmap.rect.extent.y << 1;

		if (len * cmd->srcBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		/* bitblt each plane */
		regcmd->srcBitmap.depth = 1;
		pmask = regcmd->planemask;
		regcmd->fore_color = 0xff;
		regcmd->aux_color = 0;
		if (mode == 2) {	/* N to 1 */
			for (i = 0; i < cmd->srcBitmap.depth; i++)
				if (pmask & (1 << i))
					break;
			if (i >= cmd->srcBitmap.depth)
				return 0;
			regcmd->srcBitmap.base += (len >> 1) * i;
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}
		/* else (N to N) */
		for (i = 0; i < cmd->srcBitmap.depth; i++) {
			regcmd->planemask = pmask & (1 << i);
			if (error = dobitblt(fbp, &regcmd->srcBitmap,
						  &regcmd->destBitmap))
				return error;
			regcmd->srcBitmap.base += len >> 1;
			regcmd->planemask >>= 1;
		}
		return 0;

	case BLTTYPE(BM_MEM, BM_MEM):
		lens = cmd->srcBitmap.width * cmd->srcBitmap.rect.extent.y << 1;
		lend = cmd->destBitmap.width * cmd->destBitmap.rect.extent.y << 1;
		if (lens * cmd->srcBitmap.depth <= FB_MAX_IO &&
		    lend * cmd->destBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		regcmd->srcBitmap.depth = 1;
		regcmd->destBitmap.depth = 1;
		pmask = regcmd->planemask;
		if (mode == 2) {	/* N to 1 */
			regcmd->fore_color = 0xff;
			regcmd->aux_color = 0;
			for (i = 0; i < cmd->srcBitmap.depth; i++)
				if (pmask & (1 << i))
					break;
			if (i >= cmd->srcBitmap.depth)
				return 0;
			regcmd->srcBitmap.base += (lens >> 1) * i;
			error = dobitblt(fbp, &regcmd->srcBitmap,
					      &regcmd->destBitmap);
			return error;
		} else if (mode == 1) {	/* 1 to N */
			for (i = 0; i < cmd->srcBitmap.depth; i++) {
				if (error = dobitblt(fbp, &regcmd->srcBitmap,
							  &regcmd->destBitmap))
					return error;
				regcmd->planemask >>= 1;
				regcmd->fore_color >>= 1;
				regcmd->aux_color >>= 1;
				regcmd->destBitmap.base += lend >> 1;
			}
			return 0;
		} else {		/* N to N */
			regcmd->fore_color = 0xff;
			regcmd->aux_color = 0;
			for (i = 0; i < cmd->srcBitmap.depth; i++) {
				if (error = dobitblt(fbp, &regcmd->srcBitmap,
							  &regcmd->destBitmap))
					return error;
				regcmd->srcBitmap.base += lens >> 1;
				regcmd->destBitmap.base += lend >> 1;
				regcmd->planemask >>= 1;
			}
			return 0;
		}
		break;

	default:
		return EINVAL;
	}

	return error;
}

fbnbatchbitblt(fbp, cmd, seg)
	register struct fbreg *fbp;
	register lBatchBitblt *cmd;
	int seg;
{
	register int error;
	register int mode;
	register int len;

#ifdef CPU_DOUBLE
	int blttype = BLTTYPE(cmd->srcBitmap.type, cmd->destBitmap.type);

	if ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_1, BM_MEM))) {
		return(mfbnbatchbitblt(fbp, cmd, seg));
		/* notreached */
	}
#endif

	fbinitlock();
	fbp->fb_command = FB_CBATCHBITBLT;
	fbp->fb_batchbitblt = *cmd;

	if (error = checkbitmap(&cmd->srcBitmap))
		return error;
	if (error = checkbitmap(&cmd->destBitmap))
		return error;
	if ((mode = checkdepth(&cmd->srcBitmap, &cmd->destBitmap)) < 0)
		return EINVAL;

#ifdef CPU_SINGLE
	if ((len = cmd->nSrcDest * sizeof(lSrcDest)) <= 0)
		return EINVAL;
	if (error = fblockmem((caddr_t)cmd->srcDestList,
			      len, B_WRITE, fbmap + 2, seg))
		return error;
	fbp->fb_batchbitblt.srcDestList = (lSrcDest *)ipc_phys(fbmap + 2);
	error = procbatchbitblt(fbp, mode, cmd);
#else
	fbp->fb_batchbitblt.srcDestList = (lSrcDest*)ipc_phys(srcdestlist);
	while(cmd->nSrcDest > 0) {
		len = min(cmd->nSrcDest, (MAX_SIZE / sizeof(lSrcDest)));
		error = COPYIN((caddr_t)cmd->srcDestList, (caddr_t)srcdestlist,
				len * sizeof(lSrcDest), seg);
		if (error)
			return error;
		cmd->nSrcDest -= len;
		cmd->srcDestList += len;
		fbp->fb_batchbitblt.nSrcDest = len;

		if (error = procbatchbitblt(fbp, mode, cmd))
			return error;
	}
#endif /* CPU_DOUBLE */
	return error;
}

fbbatchbitblt(fbp, cmd)
	struct fbreg *fbp;
	register sBatchBitblt *cmd;
{
	lBatchBitblt lcmd;
	register lBatchBitblt *lcmdp;
	static lSrcDest ls[100];
	register lSrcDest *lp;
	register sSrcDest *sp;
	register int ns;

	lcmdp = &lcmd;

	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->srcBitmap.type = cmd->srcBitmap.type;
	lcmdp->srcBitmap.depth = cmd->srcBitmap.depth;
	lcmdp->srcBitmap.width = cmd->srcBitmap.width;
	lcmdp->srcBitmap.rect.origin.x = cmd->srcBitmap.rect.origin.x;
	lcmdp->srcBitmap.rect.origin.y = cmd->srcBitmap.rect.origin.y;
	lcmdp->srcBitmap.rect.extent.x = cmd->srcBitmap.rect.extent.x;
	lcmdp->srcBitmap.rect.extent.y = cmd->srcBitmap.rect.extent.y;
	lcmdp->srcBitmap.base = cmd->srcBitmap.base;
	lcmdp->destBitmap.type = cmd->destBitmap.type;
	lcmdp->destBitmap.depth = cmd->destBitmap.depth;
	lcmdp->destBitmap.width = cmd->destBitmap.width;
	lcmdp->destBitmap.rect.origin.x = cmd->destBitmap.rect.origin.x;
	lcmdp->destBitmap.rect.origin.y = cmd->destBitmap.rect.origin.y;
	lcmdp->destBitmap.rect.extent.x = cmd->destBitmap.rect.extent.x;
	lcmdp->destBitmap.rect.extent.y = cmd->destBitmap.rect.extent.y;
	lcmdp->destBitmap.base = cmd->destBitmap.base;
	lcmdp->destClip.origin.x = cmd->destClip.origin.x;
	lcmdp->destClip.origin.y = cmd->destClip.origin.y;
	lcmdp->destClip.extent.x = cmd->destClip.extent.x;
	lcmdp->destClip.extent.y = cmd->destClip.extent.y;
	lcmdp->srcDestList = ls;
	sp = (sSrcDest *)cmd->srcDestList;
	while (cmd->nSrcDest) {
		lcmdp->nSrcDest = ns = min(cmd->nSrcDest, 100);
		cmd->nSrcDest -= ns;
		lp = ls;
		while (ns-- > 0) {
			int error;
			sSrcDest tmp;

			error = copyin((caddr_t)sp, (caddr_t)&tmp, sizeof(tmp));
			if (error)
				return (error);
			lp->srcRect.origin.x = tmp.srcRect.origin.x;
			lp->srcRect.origin.y = tmp.srcRect.origin.y;
			lp->srcRect.extent.x = tmp.srcRect.extent.x;
			lp->srcRect.extent.y = tmp.srcRect.extent.y;
			lp->destPoint.x = tmp.destPoint.x;
			lp->destPoint.y = tmp.destPoint.y;
			lp++;
			sp++;
		}
		fbnbatchbitblt(fbp, lcmdp, UIO_SYSSPACE);
	}
}

fbntilebitblt(fbp, cmd)
	register struct fbreg *fbp;
	register lTileBitblt *cmd;
{
	register lTileBitblt *regcmd;
	register int len, lens, lend;
	register int i;
	int mode;
	int pmask;
	int error;
	int blttype = BLTTYPE(cmd->ptnBitmap.type, cmd->destBitmap.type);

#ifdef CPU_DOUBLE
	if ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_1, BM_MEM))) {
		return(mfbntilebitblt(fbp, cmd));
		/* NOTREACHED */
	}
#endif

	if (error = checkbitmap(&cmd->ptnBitmap))
		return error;
	if (error = checkbitmap(&cmd->destBitmap))
		return error;
	if ((mode = checkdepth(&cmd->ptnBitmap, &cmd->destBitmap)) < 0)
		return EINVAL;

	fbp->fb_command = FB_CTILEBITBLT;
	fbp->fb_tilebitblt = *cmd;
	regcmd = &fbp->fb_tilebitblt;

	/* process bitblt command */
	switch (blttype) {
	case BLTTYPE(BM_FB, BM_FB):
	case BLTTYPE(BM_0, BM_FB):
	case BLTTYPE(BM_1, BM_FB):
		fbstart(fbp, 0);
		break;

	case BLTTYPE(BM_FB, BM_MEM):
	case BLTTYPE(BM_0, BM_MEM):
	case BLTTYPE(BM_1, BM_MEM):
		len = cmd->destBitmap.width * cmd->destBitmap.rect.extent.y << 1;
		if (len * cmd->destBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->ptnBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		/* bitblt each plane */
		regcmd->destBitmap.depth = 1;
		pmask = regcmd->planemask;
		for (i = 0; i < cmd->destBitmap.depth; i++) {
			if (mode == 3)	/* N to N */
				regcmd->planemask = pmask & (1 << i);

			if (error = dobitblt(fbp, &regcmd->ptnBitmap,
						  &regcmd->destBitmap))
				return error;

			regcmd->destBitmap.base += len >> 1;
			if (mode == 1) {	/* N to N */
				regcmd->planemask >>= 1;
				regcmd->fore_color >>= 1;
				regcmd->aux_color >>= 1;
			}
		}
		break;

	case BLTTYPE(BM_MEM, BM_FB):
		len = cmd->ptnBitmap.width * cmd->ptnBitmap.rect.extent.y << 1;

		if (len * cmd->ptnBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->ptnBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		/* bitblt each plane */
		regcmd->ptnBitmap.depth = 1;
		pmask = regcmd->planemask;
		regcmd->fore_color = 0xff;
		regcmd->aux_color = 0;
		if (mode == 2) {	/* N to 1 */
			for (i = 0; i < cmd->ptnBitmap.depth; i++)
				if (pmask & (1 << i))
					break;
			if (i >= cmd->ptnBitmap.depth)
				return 0;
			regcmd->ptnBitmap.base += (len >> 1) * i;
			error = dobitblt(fbp, &regcmd->ptnBitmap,
					      &regcmd->destBitmap);
			return error;
		}
		/* else (N to N) */
		for (i = 0; i < cmd->ptnBitmap.depth; i++) {
			regcmd->planemask = pmask & (1 << i);
			if (error = dobitblt(fbp, &regcmd->ptnBitmap,
						  &regcmd->destBitmap))
				return error;
			regcmd->ptnBitmap.base += len >> 1;
			regcmd->planemask >>= 1;
		}
		return 0;

	case BLTTYPE(BM_MEM, BM_MEM):
		lens = cmd->ptnBitmap.width * cmd->ptnBitmap.rect.extent.y << 1;
		lend = cmd->destBitmap.width * cmd->destBitmap.rect.extent.y << 1;
		if (lens * cmd->ptnBitmap.depth <= FB_MAX_IO &&
		    lend * cmd->destBitmap.depth <= FB_MAX_IO) {
			error = dobitblt(fbp, &regcmd->ptnBitmap,
					      &regcmd->destBitmap);
			return error;
		}

		regcmd->ptnBitmap.depth = 1;
		regcmd->destBitmap.depth = 1;
		pmask = regcmd->planemask;
		if (mode == 2) {	/* N to 1 */
			regcmd->fore_color = 0xff;
			regcmd->aux_color = 0;
			for (i = 0; i < cmd->ptnBitmap.depth; i++)
				if (pmask & (1 << i))
					break;
			if (i >= cmd->ptnBitmap.depth)
				return 0;
			regcmd->ptnBitmap.base += (lens >> 1) * i;
			error = dobitblt(fbp, &regcmd->ptnBitmap,
					      &regcmd->destBitmap);
			return error;
		} else if (mode == 1) {	/* 1 to N */
			for (i = 0; i < cmd->ptnBitmap.depth; i++) {
				if (error = dobitblt(fbp, &regcmd->ptnBitmap,
							  &regcmd->destBitmap))
					return error;
				regcmd->planemask >>= 1;
				regcmd->fore_color >>= 1;
				regcmd->aux_color >>= 1;
				regcmd->destBitmap.base += lend >> 1;
			}
			return 0;
		} else {		/* N to N */
			for (i = 0; i < cmd->ptnBitmap.depth; i++) {
				if (error = dobitblt(fbp, &regcmd->ptnBitmap,
							  &regcmd->destBitmap))
					return error;
				regcmd->ptnBitmap.base += lens >> 1;
				regcmd->destBitmap.base += lend >> 1;
				regcmd->planemask >>= 1;
			}
			return 0;
		}
		break;

	default:
		return EINVAL;
	}
	return error;
}

fbtilebitblt(fbp, cmd)
	struct fbreg *fbp;
	register sTileBitblt *cmd;
{
	lTileBitblt lcmd;
	register lTileBitblt *lcmdp;

	lcmdp = &lcmd;

	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->ptnBitmap.type = cmd->ptnBitmap.type;
	lcmdp->ptnBitmap.depth = cmd->ptnBitmap.depth;
	lcmdp->ptnBitmap.width = cmd->ptnBitmap.width;
	lcmdp->ptnBitmap.rect.origin.x = cmd->ptnBitmap.rect.origin.x;
	lcmdp->ptnBitmap.rect.origin.y = cmd->ptnBitmap.rect.origin.y;
	lcmdp->ptnBitmap.rect.extent.x = cmd->ptnBitmap.rect.extent.x;
	lcmdp->ptnBitmap.rect.extent.y = cmd->ptnBitmap.rect.extent.y;
	lcmdp->ptnBitmap.base = cmd->ptnBitmap.base;
	lcmdp->ptnRect.origin.x = cmd->ptnRect.origin.x;
	lcmdp->ptnRect.origin.y = cmd->ptnRect.origin.y;
	lcmdp->ptnRect.extent.x = cmd->ptnRect.extent.x;
	lcmdp->ptnRect.extent.y = cmd->ptnRect.extent.y;
	lcmdp->refPoint.x = cmd->refPoint.x;
	lcmdp->refPoint.y = cmd->refPoint.y;
	lcmdp->destBitmap.type = cmd->destBitmap.type;
	lcmdp->destBitmap.depth = cmd->destBitmap.depth;
	lcmdp->destBitmap.width = cmd->destBitmap.width;
	lcmdp->destBitmap.rect.origin.x = cmd->destBitmap.rect.origin.x;
	lcmdp->destBitmap.rect.origin.y = cmd->destBitmap.rect.origin.y;
	lcmdp->destBitmap.rect.extent.x = cmd->destBitmap.rect.extent.x;
	lcmdp->destBitmap.rect.extent.y = cmd->destBitmap.rect.extent.y;
	lcmdp->destBitmap.base = cmd->destBitmap.base;
	lcmdp->destClip.origin.x = cmd->destClip.origin.x;
	lcmdp->destClip.origin.y = cmd->destClip.origin.y;
	lcmdp->destClip.extent.x = cmd->destClip.extent.x;
	lcmdp->destClip.extent.y = cmd->destClip.extent.y;
	lcmdp->destRect.origin.x = cmd->destRect.origin.x;
	lcmdp->destRect.origin.y = cmd->destRect.origin.y;
	lcmdp->destRect.extent.x = cmd->destRect.extent.x;
	lcmdp->destRect.extent.y = cmd->destRect.extent.y;
	return (fbntilebitblt(fbp, lcmdp));
}

/* ARGSUSED */
fbbitblt3(fbp, cmd)
	struct fbreg *fbp;
	sBitblt3 *cmd;
{
	return ENXIO;
}

/* ARGSUSED */
fbnbitblt3(fbp, cmd)
	struct fbreg *fbp;
	lBitblt3 *cmd;
{
	return ENXIO;
}

fbnpolyline(fbp, cmd, dj, seg)
	struct fbreg *fbp;
	register lPrimLine *cmd;
	int dj;			/* if not zero, disjoint polyline */
	int seg;
{
	register int error = 0;
	register int len;

#ifdef CPU_DOUBLE
	if(cmd->drawBM.type == BM_MEM) {
		return(mfbnpolyline(fbp, cmd, dj, seg));
		/* NOTREACHED */
	}
#endif

	fbinitlock();
	fbp->fb_command = dj ? FB_CDJPOLYLINE : FB_CPOLYLINE;
	fbp->fb_polyline = *cmd;

	if ((cmd->np & 1) && dj)
		return EINVAL;

#ifdef CPU_SINGLE
	if (error = fblocksbitmap(&fbp->fb_polyline.drawBM, B_READ, fbmap))
		return error;
	if ((len = cmd->np * sizeof(lPoint)) <= 0)
		return EINVAL;
	error = fblockmem((caddr_t)cmd->plist, len, B_WRITE, fbmap + 1, seg);
	if (error)
		return error;
	fbp->fb_polyline.plist = (lPoint *)ipc_phys(fbmap + 1);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	fbp->fb_polyline.plist = (lPoint *)ipc_phys(srcdestlist);
	while (cmd->np > 0) {
		len = min(cmd->np, ((MAX_SIZE / sizeof(lPoint)) & ~1));
		fbp->fb_polyline.np = len;
		if (error = COPYIN((caddr_t)cmd->plist, (caddr_t)srcdestlist,
				   len * sizeof(lPoint), seg)) {
			return error;
		}
		cmd->np -= len;
		cmd->plist += len;

		if (fbp->fb_polyline.drawBM.type == BM_MEM) {
			if (error = fblocksbitmap(&fbp->fb_polyline.drawBM,
						  B_READ, fbmap)) {
				return error;
			}
			fbdolock();
			fbstart(fbp, 1);
			fbunlock();
		} else if (cmd->np)
			fbstart(fbp, 1);
		else
			fbstart(fbp, 0);
		if (!dj && cmd->np) {
			cmd->np++;
			cmd->plist--;
		}
	}
#endif /* CPU_SINGLE */
	return error;
}

fbpolyline(fbp, cmd, dj)
	struct fbreg *fbp;
	register sPrimLine *cmd;
	int dj;
{
	lPrimLine lcmd;
	register lPrimLine *lcmdp;
	static lPoint 	pl[100];
	register lPoint *lp;
	register sPoint *sp;
	register int	np;

	lcmdp = &lcmd;

	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->drawBM.type = cmd->drawBM.type;
	lcmdp->drawBM.depth = cmd->drawBM.depth;
	lcmdp->drawBM.width = cmd->drawBM.width;
	lcmdp->drawBM.rect.origin.x = cmd->drawBM.rect.origin.x;
	lcmdp->drawBM.rect.origin.y = cmd->drawBM.rect.origin.y;
	lcmdp->drawBM.rect.extent.x = cmd->drawBM.rect.extent.x;
	lcmdp->drawBM.rect.extent.y = cmd->drawBM.rect.extent.y;
	lcmdp->drawBM.base = cmd->drawBM.base;
	lcmdp->clip.origin.x = cmd->clip.origin.x;
	lcmdp->clip.origin.y = cmd->clip.origin.y;
	lcmdp->clip.extent.x = cmd->clip.extent.x;
	lcmdp->clip.extent.y = cmd->clip.extent.y;
	lcmdp->lptn = cmd->lptn;
	lcmdp->dlpf = cmd->dlpf;
	lcmdp->plist = pl;
	sp = (sPoint *)cmd->plist;
	while (cmd->np) {
		lcmdp->np = np = min(cmd->np, 100);
		cmd->np -= np;
		lp = pl;
		while (np-- > 0) {
			int error;
			sPoint tmp;

			if (error = copyin((caddr_t)sp, (caddr_t)&tmp, sizeof (tmp)))
				return (error);
			lp->x = tmp.x;
			lp->y = tmp.y;
			lp++;
			sp++;
		}
		fbnpolyline(fbp, lcmdp, dj, UIO_SYSSPACE);
	}
}

fbnfillscan(fbp, cmd, seg)
	struct fbreg *fbp;
	register lPrimFill *cmd;
	int seg;
{
	register int error;
	register int len;

#ifdef CPU_DOUBLE
	int blttype = BLTTYPE(cmd->ptnBM.type, cmd->drawBM.type);

	if ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_1, BM_MEM))) {
		return(mfbnfillscan(fbp, cmd, seg));
		/* NOTREACHED */
	}
#endif

	fbinitlock();
	fbp->fb_command = FB_CFILLSCAN;
	fbp->fb_fillscan = *cmd;

#ifdef CPU_SINGLE
	if (error = fblocksbitmap(&fbp->fb_fillscan.ptnBM, B_WRITE, fbmap))
		return error;
	if (error = fblocksbitmap(&fbp->fb_fillscan.drawBM, B_READ, fbmap + 1))
		return error;
	if ((len = cmd->nscan * sizeof(lScanl)) <= 0)
		return EINVAL;
	if (error = fblockmem(cmd->scan, len, B_WRITE, fbmap + 2, seg))
		return error;
	fbp->fb_fillscan.scan = (lScanl *)ipc_phys(fbmap + 2);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	fbp->fb_fillscan.scan = (lScanl *)ipc_phys(srcdestlist);
	while (cmd->nscan > 0) {
		len = min(cmd->nscan, (MAX_SIZE / sizeof(lScanl)));
		fbp->fb_fillscan.nscan = len;
		if (error = COPYIN((caddr_t)cmd->scan, (caddr_t)srcdestlist,
				   len * sizeof(lScanl), seg)) {
			return error;
		}
		cmd->nscan -= len;
		cmd->scan += len;
		if (fbp->fb_fillscan.ptnBM.type == BM_MEM ||
		    fbp->fb_fillscan.drawBM.type == BM_MEM) {

			if (error = fblocksbitmap(&fbp->fb_fillscan.ptnBM,
						  B_WRITE, fbmap)) {
				return error;
			}
			if (error = fblocksbitmap(&fbp->fb_fillscan.drawBM,
						  B_READ, fbmap + 1)) {
				return error;
			}

			fbdolock();
			fbstart(fbp, 1);
			fbunlock();
		} else if (cmd->nscan)
			fbstart(fbp, 1);
		else
			fbstart(fbp, 0);
	}
#endif /* CPU_SINGLE */
	return error;
}

fbfillscan(fbp, cmd)
	struct fbreg *fbp;
	register sPrimFill *cmd;
{
	lPrimFill lcmd;
	register lPrimFill *lcmdp;
	static lScanl	ls[100];
	register lScanl	*lp;
	register sScanl	*sp;
	register int	ns;

	lcmdp = &lcmd;
	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->refPoint.x = cmd->refPoint.x;
	lcmdp->refPoint.y = cmd->refPoint.y;
	lcmdp->ptnRect.origin.x = cmd->ptnRect.origin.x;
	lcmdp->ptnRect.origin.y = cmd->ptnRect.origin.y;
	lcmdp->ptnRect.extent.x = cmd->ptnRect.extent.x;
	lcmdp->ptnRect.extent.y = cmd->ptnRect.extent.y;
	lcmdp->ptnBM.type = cmd->ptnBM.type;
	lcmdp->ptnBM.depth = cmd->ptnBM.depth;
	lcmdp->ptnBM.width = cmd->ptnBM.width;
	lcmdp->ptnBM.rect.origin.x = cmd->ptnBM.rect.origin.x;
	lcmdp->ptnBM.rect.origin.y = cmd->ptnBM.rect.origin.y;
	lcmdp->ptnBM.rect.extent.x = cmd->ptnBM.rect.extent.x;
	lcmdp->ptnBM.rect.extent.y = cmd->ptnBM.rect.extent.y;
	lcmdp->ptnBM.base = cmd->ptnBM.base;
	lcmdp->drawBM.type = cmd->drawBM.type;
	lcmdp->drawBM.depth = cmd->drawBM.depth;
	lcmdp->drawBM.width = cmd->drawBM.width;
	lcmdp->drawBM.rect.origin.x = cmd->drawBM.rect.origin.x;
	lcmdp->drawBM.rect.origin.y = cmd->drawBM.rect.origin.y;
	lcmdp->drawBM.rect.extent.x = cmd->drawBM.rect.extent.x;
	lcmdp->drawBM.rect.extent.y = cmd->drawBM.rect.extent.y;
	lcmdp->drawBM.base = cmd->drawBM.base;
	lcmdp->clip.origin.x = cmd->clip.origin.x;
	lcmdp->clip.origin.y = cmd->clip.origin.y;
	lcmdp->clip.extent.x = cmd->clip.extent.x;
	lcmdp->clip.extent.y = cmd->clip.extent.y;
	lcmdp->scan = ls;
	sp = (sScanl *)cmd->scan;
	while (cmd->nscan) {
		lcmdp->nscan = ns = min(cmd->nscan, 100);
		cmd->nscan -= ns;
		lp = ls;
		while (ns-- > 0) {
			int error;
			sScanl tmp;

			if (error = copyin((caddr_t)sp, (caddr_t)&tmp, sizeof (tmp)))
				return (error);
			lp->x0 = tmp.x0;
			lp->x1 = tmp.x1;
			lp->y = tmp.y;
			lp++;
			sp++;
		}
		fbnfillscan(fbp, lcmdp, UIO_SYSSPACE);
	}
}

fbnrectangle(fbp, cmd)
	struct fbreg *fbp;
	register lPrimRect *cmd;
{
	register int error = 0;

#ifdef CPU_DOUBLE
	int blttype = BLTTYPE(cmd->ptnBM.type, cmd->drawBM.type);

	if ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_1, BM_MEM))) {
		return(mfbnrectangle(fbp, cmd));
		/* NOTREACHED */
	}
#endif /* CPU_DOUBLE */

	fbinitlock();
	fbp->fb_command = FB_CRECTANGLE;
	fbp->fb_rectangle = *cmd;

	if (error = fblocksbitmap(&fbp->fb_rectangle.drawBM, B_READ, fbmap))
		return error;
	if (error = fblocksbitmap(&fbp->fb_rectangle.ptnBM, B_WRITE, fbmap + 1))
		return error;

	if (fbp->fb_rectangle.drawBM.type == BM_MEM ||
	    fbp->fb_rectangle.ptnBM.type == BM_MEM) {
		fbdolock();
		fbstart(fbp, 1);
		fbunlock();
	} else {
		fbstart(fbp, 0);
	}

	return error;
}

fbrectangle(fbp, cmd)
	struct fbreg *fbp;
	register sPrimRect *cmd;
{
	lPrimRect lcmd;
	register lPrimRect *lcmdp;

	lcmdp = &lcmd;

	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->rect.origin.x = cmd->rect.origin.x;
	lcmdp->rect.origin.y = cmd->rect.origin.y;
	lcmdp->rect.extent.x = cmd->rect.extent.x;
	lcmdp->rect.extent.y = cmd->rect.extent.y;
	lcmdp->refPoint.x = cmd->refPoint.x;
	lcmdp->refPoint.y = cmd->refPoint.y;
	lcmdp->ptnRect.origin.x = cmd->ptnRect.origin.x;
	lcmdp->ptnRect.origin.y = cmd->ptnRect.origin.y;
	lcmdp->ptnRect.extent.x = cmd->ptnRect.extent.x;
	lcmdp->ptnRect.extent.y = cmd->ptnRect.extent.y;
	lcmdp->ptnBM.type = cmd->ptnBM.type;
	lcmdp->ptnBM.depth = cmd->ptnBM.depth;
	lcmdp->ptnBM.width = cmd->ptnBM.width;
	lcmdp->ptnBM.rect.origin.x = cmd->ptnBM.rect.origin.x;
	lcmdp->ptnBM.rect.origin.y = cmd->ptnBM.rect.origin.y;
	lcmdp->ptnBM.rect.extent.x = cmd->ptnBM.rect.extent.x;
	lcmdp->ptnBM.rect.extent.y = cmd->ptnBM.rect.extent.y;
	lcmdp->ptnBM.base = cmd->ptnBM.base;
	lcmdp->drawBM.type = cmd->drawBM.type;
	lcmdp->drawBM.depth = cmd->drawBM.depth;
	lcmdp->drawBM.width = cmd->drawBM.width;
	lcmdp->drawBM.rect.origin.x = cmd->drawBM.rect.origin.x;
	lcmdp->drawBM.rect.origin.y = cmd->drawBM.rect.origin.y;
	lcmdp->drawBM.rect.extent.x = cmd->drawBM.rect.extent.x;
	lcmdp->drawBM.rect.extent.y = cmd->drawBM.rect.extent.y;
	lcmdp->drawBM.base = cmd->drawBM.base;
	lcmdp->clip.origin.x = cmd->clip.origin.x;
	lcmdp->clip.origin.y = cmd->clip.origin.y;
	lcmdp->clip.extent.x = cmd->clip.extent.x;
	lcmdp->clip.extent.y = cmd->clip.extent.y;
	return (fbnrectangle(fbp, lcmdp));
}

fbnpolymarker(fbp, cmd, seg)
	register struct fbreg *fbp;
	register lPrimMarker *cmd;
	int seg;
{
	register int error;
	register int len;

#ifdef CPU_DOUBLE
	int blttype = BLTTYPE(cmd->ptnBM.type, cmd->drawBM.type);

	if ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	    (blttype == BLTTYPE(BM_1, BM_MEM))) {
		return(mfbnpolymarker(fbp, cmd, seg));
		/* NOTREACHED */
	}
#endif /* CPU_DOUBLE */

	fbinitlock();
	fbp->fb_command = FB_CPOLYMARKER;
	fbp->fb_polymarker = *cmd;

#ifdef CPU_SINGLE
	if (error = fblocksbitmap(&fbp->fb_polymarker.ptnBM, B_WRITE, fbmap))
		return error;
	if (error = fblocksbitmap(&fbp->fb_polymarker.drawBM, B_READ, fbmap+1))
		return error;
	if ((len = cmd->np * sizeof(lPoint)) <= 0)
		return EINVAL;
	if (error = fblockmem(cmd->plist, len, B_WRITE, fbmap + 2, seg))
		return error;
	fbp->fb_polymarker.plist = (lPoint *)ipc_phys(fbmap + 2);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	fbp->fb_polymarker.plist = (lPoint *)ipc_phys(srcdestlist);
	while (cmd->np > 0) {
		len = min(cmd->np, (MAX_SIZE / sizeof(lPoint)));
		fbp->fb_polymarker.np = len;
		if (error = COPYIN((caddr_t)cmd->plist, (caddr_t)srcdestlist,
				   len * sizeof(lPoint), seg)) {
			return error;
		}
		cmd->np -= len;
		cmd->plist += len;

		if (fbp->fb_polymarker.ptnBM.type == BM_MEM ||
		    fbp->fb_polymarker.drawBM.type == BM_MEM) {

			if (error = fblocksbitmap(&fbp->fb_polymarker.ptnBM,
						  B_WRITE, fbmap)) {
				return error;
			}
			if (error = fblocksbitmap(&fbp->fb_polymarker.drawBM,
						  B_READ, fbmap + 1)) {
				return error;
			}

			fbdolock();
			fbstart(fbp, 1);
			fbunlock();
		} else if (cmd->np)
			fbstart(fbp, 1);
		else
			fbstart(fbp, 0);
	}
#endif /* CPU_SINGLE */
	return error;
}

fbpolymarker(fbp, cmd)
	struct fbreg *fbp;
	register sPrimMarker *cmd;
{
	lPrimMarker lcmd;
	register lPrimMarker *lcmdp;
	static lPoint pl[100];
	register lPoint *lp;
	register sPoint *sp;
	register int np;

	lcmdp = &lcmd;
	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->ptnRect.origin.x = cmd->ptnRect.origin.x;
	lcmdp->ptnRect.origin.y = cmd->ptnRect.origin.y;
	lcmdp->ptnRect.extent.x = cmd->ptnRect.extent.x;
	lcmdp->ptnRect.extent.y = cmd->ptnRect.extent.y;
	lcmdp->ptnBM.type = cmd->ptnBM.type;
	lcmdp->ptnBM.depth = cmd->ptnBM.depth;
	lcmdp->ptnBM.width = cmd->ptnBM.width;
	lcmdp->ptnBM.rect.origin.x = cmd->ptnBM.rect.origin.x;
	lcmdp->ptnBM.rect.origin.y = cmd->ptnBM.rect.origin.y;
	lcmdp->ptnBM.rect.extent.x = cmd->ptnBM.rect.extent.x;
	lcmdp->ptnBM.rect.extent.y = cmd->ptnBM.rect.extent.y;
	lcmdp->ptnBM.base = cmd->ptnBM.base;
	lcmdp->drawBM.type = cmd->drawBM.type;
	lcmdp->drawBM.depth = cmd->drawBM.depth;
	lcmdp->drawBM.width = cmd->drawBM.width;
	lcmdp->drawBM.rect.origin.x = cmd->drawBM.rect.origin.x;
	lcmdp->drawBM.rect.origin.y = cmd->drawBM.rect.origin.y;
	lcmdp->drawBM.rect.extent.x = cmd->drawBM.rect.extent.x;
	lcmdp->drawBM.rect.extent.y = cmd->drawBM.rect.extent.y;
	lcmdp->drawBM.base = cmd->drawBM.base;
	lcmdp->clip.origin.x = cmd->clip.origin.x;
	lcmdp->clip.origin.y = cmd->clip.origin.y;
	lcmdp->clip.extent.x = cmd->clip.extent.x;
	lcmdp->clip.extent.y = cmd->clip.extent.y;
	lcmdp->plist = pl;
	sp = (sPoint *)cmd->plist;
	while (cmd->np) {
		lcmdp->np = np = min(cmd->np, 100);
		cmd->np -= np;
		while (np-- > 0) {
			int error;
			sPoint tmp;

			if (error = copyin((caddr_t)sp, (caddr_t)&tmp, sizeof (tmp)))
				return (error);
			lp->x = tmp.x;
			lp->y = tmp.y;
			lp++;
			sp++;
		}
		fbnpolymarker(fbp, lcmdp, UIO_SYSSPACE);
	}
}

fbnpolydot(fbp, cmd, seg)
	register struct fbreg *fbp;
	register lPrimDot *cmd;
	int seg;
{
	register int error;
	register int len;

#ifdef CPU_DOUBLE
	if (cmd->drawBM.type == BM_MEM)
		return (mfbnpolydot(fbp, cmd, seg));
#endif

	fbinitlock();
	fbp->fb_command = FB_CPOLYDOT;
	fbp->fb_polydot = *cmd;

#ifdef CPU_SINGLE
	if (error = fblocksbitmap(&fbp->fb_polydot.drawBM, B_READ, fbmap + 1))
		return error;
	if ((len = cmd->np * sizeof(lPoint)) <= 0)
		return EINVAL;
	if (error = fblockmem(cmd->plist, len, B_WRITE, fbmap + 2, seg))
		return error;
	fbp->fb_polydot.plist = (lPoint *)ipc_phys(fbmap + 2);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	fbp->fb_polydot.plist = (lPoint *)ipc_phys(srcdestlist);
	while (cmd->np > 0) {
		len = min(cmd->np, (MAX_SIZE / sizeof(lPoint)));
		fbp->fb_polydot.np = len;
		if (error = COPYIN((caddr_t)cmd->plist, (caddr_t)srcdestlist,
				   len * sizeof(lPoint), seg)) {
			return error;
		}
		cmd->np -= len;
		cmd->plist += len;

		if (fbp->fb_polydot.drawBM.type == BM_MEM) {
			if (error = fblocksbitmap(&fbp->fb_polydot.drawBM,
						  B_READ, fbmap + 1)) {
				return error;
			}

			fbdolock();
			fbstart(fbp, 1);
			fbunlock();
		} else if (cmd->np)
			fbstart(fbp, 1);
		else
			fbstart(fbp, 0);
	}
#endif /* CPU_SINGLE */
	return error;
}

fbpolydot(fbp, cmd)
	struct fbreg *fbp;
	register sPrimDot *cmd;
{
	lPrimDot lcmd;
	register lPrimDot *lcmdp;
	static lPoint	pl[100];
	register lPoint	*lp;
	register sPoint	*sp;
	register int	np;

	lcmdp = &lcmd;
	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->drawBM.type = cmd->drawBM.type;
	lcmdp->drawBM.depth = cmd->drawBM.depth;
	lcmdp->drawBM.width = cmd->drawBM.width;
	lcmdp->drawBM.rect.origin.x = cmd->drawBM.rect.origin.x;
	lcmdp->drawBM.rect.origin.y = cmd->drawBM.rect.origin.y;
	lcmdp->drawBM.rect.extent.x = cmd->drawBM.rect.extent.x;
	lcmdp->drawBM.rect.extent.y = cmd->drawBM.rect.extent.y;
	lcmdp->drawBM.base = cmd->drawBM.base;
	lcmdp->clip.origin.x = cmd->clip.origin.x;
	lcmdp->clip.origin.y = cmd->clip.origin.y;
	lcmdp->clip.extent.x = cmd->clip.extent.x;
	lcmdp->clip.extent.y = cmd->clip.extent.y;
	lcmdp->plist = pl;
	sp = (sPoint *)cmd->plist;
	while (cmd->np) {
		lcmdp->np = np = min(cmd->np, 100);
		cmd->np -= np;
		while (np-- > 0) {
			int error;
			sPoint tmp;

			if (error = copyin((caddr_t)sp, (caddr_t)&tmp, sizeof (tmp)))
				return (error);
			lp->x = tmp.x;
			lp->y = tmp.y;
			lp++;
			sp++;
		}
		fbnpolydot(fbp, lcmdp, UIO_SYSSPACE);
	}
}

fbntext(fbp, cmd)
	register struct fbreg *fbp;
	register lPrimText *cmd;
{
	register int error;

#ifdef CPU_DOUBLE
	int blttype = BLTTYPE(cmd->fontBM.type, cmd->drawBM.type);

	if ((cmd->type == ASCII) &&
	    ((blttype == BLTTYPE(BM_MEM, BM_MEM)) ||
	     (blttype == BLTTYPE(BM_0, BM_MEM)) ||
	     (blttype == BLTTYPE(BM_1, BM_MEM)))
	) {
		return(mfbntext(fbp, cmd));
		/* NOTREACHED */
	}
#endif /* CPU_DOUBLE */

	fbinitlock();
	fbp->fb_command = FB_CTEXT;
	fbp->fb_text = *cmd;


	if (error = fblocksbitmap(&fbp->fb_text.drawBM, B_READ, fbmap))
		return error;

	if (cmd->type == ASCII) {
		if (error = fblocksbitmap(&fbp->fb_text.fontBM,
					  B_WRITE, fbmap + 1)) {
			return error;
		}
	}

#ifdef CPU_SINGLE
	if (error = fblockmem(cmd->str, cmd->len, B_WRITE, fbmap + 2, UIO_USERSPACE))
		return error;
	fbp->fb_text.str = (unsigned char *)ipc_phys(fbmap + 2);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	fbp->fb_text.str = (unsigned char *)ipc_phys(srcdestlist);
	if (error = COPYIN((caddr_t)cmd->str,
			   (caddr_t)srcdestlist, cmd->len, UIO_USERSPACE)) {
		return error;
	}

	if (fbp->fb_text.drawBM.type == BM_MEM ||
	    (cmd->type == ASCII && fbp->fb_text.fontBM.type == BM_MEM)) {
		fbdolock();
		fbstart(fbp, 1);
		fbunlock();
	} else
		fbstart(fbp, 0);
#endif /* CPU_SINGLE */
	return error;
}

fbtext(fbp, cmd)
	struct fbreg *fbp;
	register sPrimText *cmd;
{
	lPrimText lcmd;
	register lPrimText *lcmdp;

	lcmdp = &lcmd;

	lcmdp->func = cmd->func;
	lcmdp->transp = cmd->transp;
	lcmdp->fore_color = cmd->fore_color;
	lcmdp->aux_color = cmd->aux_color;
	lcmdp->planemask = cmd->planemask;
	lcmdp->type = cmd->type;
	lcmdp->p.x = cmd->p.x;
	lcmdp->p.y = cmd->p.y;
	lcmdp->dx = cmd->dx;
	lcmdp->dy = cmd->dy;
	lcmdp->ex_factor = cmd->ex_factor;
	lcmdp->fp.x = cmd->fp.x;
	lcmdp->fp.y = cmd->fp.y;
	lcmdp->width = cmd->width;
	lcmdp->height = cmd->height;
	lcmdp->column = cmd->column;
	lcmdp->first_chr = cmd->first_chr;
	lcmdp->last_chr = cmd->last_chr;
	lcmdp->fontBM.type = cmd->fontBM.type;
	lcmdp->fontBM.depth = cmd->fontBM.depth;
	lcmdp->fontBM.width = cmd->fontBM.width;
	lcmdp->fontBM.rect.origin.x = cmd->fontBM.rect.origin.x;
	lcmdp->fontBM.rect.origin.y = cmd->fontBM.rect.origin.y;
	lcmdp->fontBM.rect.extent.x = cmd->fontBM.rect.extent.x;
	lcmdp->fontBM.rect.extent.y = cmd->fontBM.rect.extent.y;
	lcmdp->fontBM.base = cmd->fontBM.base;
	lcmdp->drawBM.type = cmd->drawBM.type;
	lcmdp->drawBM.depth = cmd->drawBM.depth;
	lcmdp->drawBM.width = cmd->drawBM.width;
	lcmdp->drawBM.rect.origin.x = cmd->drawBM.rect.origin.x;
	lcmdp->drawBM.rect.origin.y = cmd->drawBM.rect.origin.y;
	lcmdp->drawBM.rect.extent.x = cmd->drawBM.rect.extent.x;
	lcmdp->drawBM.rect.extent.y = cmd->drawBM.rect.extent.y;
	lcmdp->drawBM.base = cmd->drawBM.base;
	lcmdp->clip.origin.x = cmd->clip.origin.x;
	lcmdp->clip.origin.y = cmd->clip.origin.y;
	lcmdp->clip.extent.x = cmd->clip.extent.x;
	lcmdp->clip.extent.y = cmd->clip.extent.y;
	lcmdp->len = cmd->len;
	lcmdp->str = cmd->str;
	return (fbntext(fbp, lcmdp));
}

fbsetcursor(fbp, data)
	struct fbreg *fbp;
	sCursor	*data;
{
	register struct fbreg *fbregp;

	fbregp = fbp;

	fbregp->fb_command = FB_CSETCURSOR;
	fbregp->fb_cursor.func = data->func;
	fbregp->fb_cursor.cursor_color = data->cursor_color;
	fbregp->fb_cursor.mask_color = data->mask_color;
	fbregp->fb_cursor.hot.x = data->hot.x;
	fbregp->fb_cursor.hot.y = data->hot.y;
	fbregp->fb_cursor.size.x = data->size.x;
	fbregp->fb_cursor.size.y = data->size.y;
	fbregp->fb_cursor.cursorRect.origin.x = data->cursorRect.origin.x;
	fbregp->fb_cursor.cursorRect.origin.y = data->cursorRect.origin.y;
	fbregp->fb_cursor.cursorRect.extent.x = data->cursorRect.extent.x;
	fbregp->fb_cursor.cursorRect.extent.y = data->cursorRect.extent.y;
	fbregp->fb_cursor.maskRect.origin.x = data->maskRect.origin.x;
	fbregp->fb_cursor.maskRect.origin.y = data->maskRect.origin.y;
	fbregp->fb_cursor.maskRect.extent.x = data->maskRect.extent.x;
	fbregp->fb_cursor.maskRect.extent.y = data->maskRect.extent.y;
	fbregp->fb_cursor.saveRect.origin.x = data->saveRect.origin.x;
	fbregp->fb_cursor.saveRect.origin.y = data->saveRect.origin.y;
	fbregp->fb_cursor.saveRect.extent.x = data->saveRect.extent.x;
	fbregp->fb_cursor.saveRect.extent.y = data->saveRect.extent.y;
	fbregp->fb_cursor.moveArea.origin.x = data->moveArea.origin.x;
	fbregp->fb_cursor.moveArea.origin.y = data->moveArea.origin.y;
	fbregp->fb_cursor.moveArea.extent.x = data->moveArea.extent.x;
	fbregp->fb_cursor.moveArea.extent.y = data->moveArea.extent.y;
	fbstart(fbp, 0);
	return 0;
}

fbnsetcursor(fbp, data)
	struct fbreg *fbp;
	lCursor	*data;
{
	register struct fbreg *fbregp;

	fbregp = fbp;

	fbregp->fb_command = FB_CSETCURSOR;
	fbregp->fb_cursor.func = data->func;
	fbregp->fb_cursor.cursor_color = data->cursor_color;
	fbregp->fb_cursor.mask_color = data->mask_color;
	fbregp->fb_cursor.hot.x = data->hot.x;
	fbregp->fb_cursor.hot.y = data->hot.y;
	fbregp->fb_cursor.size.x = data->size.x;
	fbregp->fb_cursor.size.y = data->size.y;
	fbregp->fb_cursor.cursorRect.origin.x = data->cursorRect.origin.x;
	fbregp->fb_cursor.cursorRect.origin.y = data->cursorRect.origin.y;
	fbregp->fb_cursor.cursorRect.extent.x = data->cursorRect.extent.x;
	fbregp->fb_cursor.cursorRect.extent.y = data->cursorRect.extent.y;
	fbregp->fb_cursor.maskRect.origin.x = data->maskRect.origin.x;
	fbregp->fb_cursor.maskRect.origin.y = data->maskRect.origin.y;
	fbregp->fb_cursor.maskRect.extent.x = data->maskRect.extent.x;
	fbregp->fb_cursor.maskRect.extent.y = data->maskRect.extent.y;
	fbregp->fb_cursor.saveRect.origin.x = data->saveRect.origin.x;
	fbregp->fb_cursor.saveRect.origin.y = data->saveRect.origin.y;
	fbregp->fb_cursor.saveRect.extent.x = data->saveRect.extent.x;
	fbregp->fb_cursor.saveRect.extent.y = data->saveRect.extent.y;
	fbregp->fb_cursor.moveArea.origin.x = data->moveArea.origin.x;
	fbregp->fb_cursor.moveArea.origin.y = data->moveArea.origin.y;
	fbregp->fb_cursor.moveArea.extent.x = data->moveArea.extent.x;
	fbregp->fb_cursor.moveArea.extent.y = data->moveArea.extent.y;
	fbstart(fbp, 0);
	return 0;
}

fbgetscrtype(fbp, data)
	struct fbreg *fbp;
	sScrType *data;
{
	fbp->fb_command = FB_CGETSCRTYPE;
	fbstart(fbp, 1);
	data->colorwidth = fbp->fb_scrtype.colorwidth;
	data->plane = fbp->fb_scrtype.plane;
	data->bufferrect.origin.x = fbp->fb_scrtype.bufferrect.origin.x;
	data->bufferrect.origin.y = fbp->fb_scrtype.bufferrect.origin.y;
	data->bufferrect.extent.x = fbp->fb_scrtype.bufferrect.extent.x;
	data->bufferrect.extent.y = fbp->fb_scrtype.bufferrect.extent.y;
	data->visiblerect.origin.x = fbp->fb_scrtype.visiblerect.origin.x;
	data->visiblerect.origin.y = fbp->fb_scrtype.visiblerect.origin.y;
	data->visiblerect.extent.x = fbp->fb_scrtype.visiblerect.extent.x;
	data->visiblerect.extent.y = fbp->fb_scrtype.visiblerect.extent.y;
	return 0;
}

fbsetxy(fbp, data)
	struct fbreg *fbp;
	sPoint *data;
{
	fbp->fb_command = FB_CSETXY;
	fbp->fb_point.x = data->x;
	fbp->fb_point.y = data->y;
	fbstart(fbp, 0);
	return 0;
}

fbsetpalette(fbp, data)
	struct fbreg *fbp;
	sPalette *data;
{
	lPalette	pal;

	fbp->fb_command = FB_CSETPALETTE;
	fbp->fb_palette.count = 1;
	*(sPalette *)srcdestlist = *data;
#ifdef CPU_SINGLE
	fbmap[0].fm_vaddr = (caddr_t)srcdestlist;
	fbmap[0].fm_offset = 0;
	fbp->fb_palette.palette = (sPalette *)ipc_phys(fbmap);
#else
	fbp->fb_palette.palette = (sPalette *)ipc_phys(srcdestlist);
#endif
	fbstart(fbp, 0);
	return 0;
}

fbnsetpalette(fbp, cmd)
	struct fbreg *fbp;
	lPalette *cmd;
{
	register int error;
	register int count;
#ifdef CPU_SINGLE
	register int len;
#endif

	fbinitlock();
	fbp->fb_command = FB_CSETPALETTE;
#ifdef CPU_SINGLE
	fbp->fb_palette.count = cmd->count;
	if ((len = cmd->count * sizeof(sPalette)) <= 0)
		return EINVAL;
	if (error = fblockmem(cmd->palette, len, B_WRITE, fbmap, UIO_USERSPACE))
		return error;
	fbp->fb_palette.palette = (sPalette *)ipc_phys(fbmap);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	fbp->fb_palette.palette = (sPalette *)ipc_phys(srcdestlist);
	while (cmd->count > 0) {
		count = min(cmd->count, (MAX_SIZE / sizeof(sPalette)));
		fbp->fb_palette.count = count;
		if (error = COPYIN((caddr_t)cmd->palette, (caddr_t)srcdestlist,
				   count * sizeof(sPalette), UIO_USERSPACE)) {
			break;
		}
		cmd->count -= count;
		cmd->palette += count;

		fbstart(fbp, 0);
	}
#endif /* CPU_SINGLE */
	return error;
}

fbgetpalette(fbp, data)
	struct fbreg *fbp;
	sPalette *data;
{
	lPalette pal;

	fbp->fb_command = FB_CGETPALETTE;
	fbp->fb_palette.count = 1;
	*(sPalette *)srcdestlist = *data;
#ifdef CPU_SINGLE
	fbmap[0].fm_vaddr = (caddr_t)srcdestlist;
	fbmap[0].fm_offset = 0;
	fbp->fb_palette.palette = (sPalette *)ipc_phys(fbmap);
#else
	fbp->fb_palette.palette = (sPalette *)ipc_phys(srcdestlist);
#endif
	fbstart(fbp, 1);
#ifdef mips
	MachFlushDCache((caddr_t)srcdestlist, sizeof (sPalette));
	*data = *(sPalette *)MACH_CACHED_TO_UNCACHED(srcdestlist);
#else
	*data = *(sPalette *)srcdestlist;
#endif
	return fbp->fb_result;
}

fbngetpalette(fbp, cmd)
	struct fbreg *fbp;
	lPalette *cmd;
{
	register int error;
	register int count;
#ifdef CPU_SINGLE
	register int len;
#else
	register int savecount;
	register sPalette *savep;
#endif

	fbinitlock();
	fbp->fb_command = FB_CGETPALETTE;
#ifdef CPU_SINGLE
	fbp->fb_palette.count = cmd->count;
	if ((len = cmd->count * sizeof(sPalette)) <= 0)
		return EINVAL;
	if (error = fblockmem(cmd->palette, len, B_WRITE, fbmap, UIO_USERSPACE))
		return error;
	fbp->fb_palette.palette = (sPalette *)ipc_phys(fbmap);

	fbdolock();
	fbstart(fbp, 1);
	fbunlock();
#else /* CPU_SINGLE */
	savecount = cmd->count;
	savep = cmd->palette;
	fbp->fb_palette.palette = (sPalette *)ipc_phys(srcdestlist);
	while (cmd->count > 0) {
		count = min(cmd->count, (MAX_SIZE / sizeof(sPalette)));
		fbp->fb_palette.count = count;
		if (error = COPYIN((caddr_t)cmd->palette, (caddr_t)srcdestlist,
				   count * sizeof(sPalette), UIO_USERSPACE)) {
			break;
		}
		fbstart(fbp, 1);
#ifdef mips
		MachFlushDCache((caddr_t)srcdestlist, sizeof (sPalette));
		error = copyout((caddr_t)MACH_CACHED_TO_UNCACHED(srcdestlist),
				(caddr_t)cmd->palette,
				count * sizeof(sPalette));
#else
		error = copyout((caddr_t)srcdestlist,
				(caddr_t)cmd->palette,
				count * sizeof(sPalette));
#endif
		if (error)
			break;
		cmd->count -= count;
		cmd->palette += count;
	}
	cmd->count = savecount;
	cmd->palette = savep;
#endif /* CPU_SINGLE */
	return fbp->fb_result;
}
#endif /* NFB > 0 */
