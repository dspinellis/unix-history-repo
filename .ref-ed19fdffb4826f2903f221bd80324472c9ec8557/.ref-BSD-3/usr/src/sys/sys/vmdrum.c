/*	vmdrum.c	2.1	1/5/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/proc.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/text.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/cmap.h"

/*
 * Expand the swap area for both the data and stack segments.
 * If space is not available for both, retract and return 0.
 */
swpexpand(ds, ss, dmp, smp)
	size_t ds, ss;
	register struct dmap *dmp, *smp;
{
	register struct dmap *tmp;
	register int ts;
	size_t ods;

	/*
	 * If dmap isn't growing, do smap first.
	 * This avoids anomalies if smap will try to grow and
	 * fail, which otherwise would shrink ds without expanding
	 * ss, a rather curious side effect!
	 */
	if (dmp->dm_alloc > ds) {
		tmp = dmp; ts = ds;
		dmp = smp; ds = ss;
		smp = tmp; ss = ts;
	}
	ods = dmp->dm_size;
	if (vsexpand(ds, dmp) == 0)
		return (0);
	if (vsexpand(ss, smp) == 0) {
		VOID vsexpand(ods, dmp);
		return (0);
	}
	return (1);
}

/*
 * Expand or contract the virtual swap segment mapped
 * by the argument diskmap so as to just allow the given size.
 *
 * FOR NOW CANT RELEASE UNLESS SHRINKING TO ZERO, SINCE PAGEOUTS MAY
 * BE IN PROGRESS... TYPICALLY NEVER SHRINK ANYWAYS, SO DOESNT MATTER MUCH
 */
vsexpand(vssize, dmp)
	register size_t vssize;
	register struct dmap *dmp;
{
	register int blk = DMMIN;
	register int vsbase = 0;
	register swblk_t *ip = dmp->dm_map;
	size_t oldsize = dmp->dm_size;

	if (vssize >= dmp->dm_size && vssize <= dmp->dm_alloc) {
		dmp->dm_size = vssize;
		return (1);
	}
	while (vsbase < dmp->dm_alloc || vsbase < vssize) {
		if (vsbase >= dmp->dm_alloc) {
			*ip = malloc(swapmap, ctod(blk));
			if (*ip == 0) {
				dmp->dm_size = vsbase;
				if (vsexpand(oldsize, dmp) == 0)
					panic("vsexpand");
				return (0);
			}
			dmp->dm_alloc += blk;
		} else if (vssize == 0) {
		/* } else if (vsbase >= vssize) { */
			mfree(swapmap, ctod(blk), *ip);
			*ip = 0;
			dmp->dm_alloc -= blk;
		}
		vsbase += blk;
		if (blk < DMMAX)
			blk *= 2;
		ip++;
		if (ip - dmp->dm_map > NDMAP)
			panic("vmdrum NDMAP");
	}
	dmp->dm_size = vssize;
	return (1);
}

/*
 * Swap a segment of virtual memory to disk,
 * by locating the contiguous dirty pte's
 * and calling vschunk with each chunk.
 */
vsswap(p, pte, type, vsbase, vscount, dmp)
	struct proc *p;
	register struct pte *pte;
	int type;
	register int vsbase, vscount;
	struct dmap *dmp;
{
	register int size = 0;

	if (vscount % CLSIZE)
		panic("vsswap");
	for (;;) {
		if (vscount == 0 || !dirtycl(pte)) {
			if (size) {
				vschunk(p, vsbase, size, type, dmp);
				vsbase += size;
				size = 0;
			}
			if (vscount == 0)
				return;
			vsbase += CLSIZE;
			if (pte->pg_fod == 0 && pte->pg_pfnum)
				if (type == MTEXT)
					p->p_textp->x_rssize -= vmemfree(pte, CLSIZE);
				else
					p->p_rssize -= vmemfree(pte, CLSIZE);
		} else {
			size += CLSIZE;
			mwait(pte->pg_pfnum);
			if (anycl(pte, pg_m))
				zapcl(pte, pg_vreadm) = 1;
		}
		vscount -= CLSIZE;
		if (type == MSTACK)
			pte -= CLSIZE;
		else
			pte += CLSIZE;
	}
}

vschunk(p, base, size, type, dmp)
	register struct proc *p;
	register int base, size;
	int type;
	struct dmap *dmp;
{
	register struct pte *pte;
	struct dblock db;
	unsigned v;

	if (type == MTEXT) {
		swap(p, p->p_textp->x_daddr+base, ptob(tptov(p, base)),
	 	    ctob(size), B_WRITE, 0, swapdev);
		p->p_textp->x_rssize -= vmemfree(tptopte(p, base), size);
		return;
	}
	do {
		vstodb(base, size, dmp, &db, type == MSTACK);
		v = type==MSTACK ? sptov(p, base+db.db_size-1) : dptov(p, base);
		swap(p, db.db_base, ptob(v), ctob(db.db_size), B_WRITE, 0, swapdev);
		pte = type==MSTACK ? sptopte(p, base+db.db_size-1) : dptopte(p, base);
		p->p_rssize -= vmemfree(pte, db.db_size);
		base += db.db_size;
		size -= db.db_size;
	} while (size != 0);
}

/*
 * Given a base/size pair in virtual swap area,
 * return a physical base/size pair which is the
 * (largest) initial, physically contiguous block.
 */
vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase, vssize;
	struct dmap *dmp;
	register struct dblock *dbp;
{
	register int blk = DMMIN;
	register swblk_t *ip = dmp->dm_map;

	if (vsbase < 0 || vssize < 0 || vsbase + vssize > dmp->dm_size)
		panic("vstodb");
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < DMMAX)
			blk *= 2;
		ip++;
	}
	if (*ip + blk > nswap)
		panic("vstodb *ip");
	dbp->db_size = imin(vssize, blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + dbp->db_size) : vsbase);
}

/*
 * Convert a virtual page number 
 * to its corresponding disk block number.
 * Used in pagein/pageout to initiate single page transfers.
 */
swblk_t
vtod(p, v, dmap, smap)
	register struct proc *p;
	unsigned v;
	struct dmap *dmap, *smap;
{
	struct dblock db;

	if (isatsv(p, v))
		return (p->p_textp->x_daddr + vtotp(p, v));
	if (isassv(p, v))
		vstodb(vtosp(p, v), 1, smap, &db, 1);
	else
		vstodb(vtodp(p, v), 1, dmap, &db, 0);
	return (db.db_base);
}
