/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vm_machdep.c	7.2 (Berkeley) 7/9/88
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "cmap.h"
#include "mount.h"
#include "vm.h"
#include "text.h"
#include "kernel.h"

#include "pte.h"
#include "cpu.h"
#include "mtpr.h"

/*
 * Set a red zone in the kernel stack after the u. area.
 */
setredzone(pte, vaddr)
	register struct pte *pte;
	caddr_t vaddr;
{

	pte += (sizeof (struct user) + NBPG - 1) / NBPG;
	*(int *)pte &= ~PG_PROT;
	*(int *)pte |= PG_URKR;
	if (vaddr)
		mtpr(TBIS, vaddr + sizeof (struct user) + NBPG - 1);
}

/*
 * Check for valid program size
 * NB - Check data and data growth separately as they may overflow 
 * when summed together.
 */
chksize(ts, ids, uds, ss)
	register unsigned ts, ids, uds, ss;
{
	extern unsigned maxtsize;

	if (ctob(ts) > maxtsize ||
	    ctob(ids) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(uds) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(ids + uds) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(ss) > u.u_rlimit[RLIMIT_STACK].rlim_cur) {
		u.u_error = ENOMEM;
		return (1);
	}
	return (0);
}

/*ARGSUSED*/
newptes(pte, v, size)
	register struct pte *pte;
	u_int v;
	register int size;
{
	register caddr_t a = ptob(v);

#ifdef lint
	pte = pte;
#endif
	if (size >= 8) {
		mtpr(TBIA, 0);
		return;
	}
	while (size > 0) {
		mtpr(TBIS, a);
		a += NBPG;
		size--;
	}
}

/*
 * Change protection codes of text segment.
 * Have to flush translation buffer since this
 * affect virtual memory mapping of current process.
 */
chgprot(addr, tprot)
	caddr_t addr;
	long tprot;
{
	unsigned v;
	int tp;
	register struct pte *pte;
	register struct cmap *c;

	v = clbase(btop(addr));
	if (!isatsv(u.u_procp, v)) {
		u.u_error = EFAULT;
		return (0);
	}
	tp = vtotp(u.u_procp, v);
	pte = tptopte(u.u_procp, tp);
	if (pte->pg_fod == 0 && pte->pg_pfnum) {
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_blkno && c->c_mdev != MSWAPX)
			munhash(mount[c->c_mdev].m_dev,
			    (daddr_t)(u_long)c->c_blkno);
	}
	*(int *)pte &= ~PG_PROT;
	*(int *)pte |= tprot;
	distcl(pte);
	tbiscl(v);
	return (1);
}

settprot(tprot)
	long tprot;
{
	register int *ptaddr, i;

	ptaddr = (int *)mfpr(P0BR);
	for (i = 0; i < u.u_tsize; i++) {
		ptaddr[i] &= ~PG_PROT;
		ptaddr[i] |= tprot;
	}
	mtpr(TBIA, 0);
}

#ifdef notdef
/*
 * Rest are machine-dependent
 */
getmemc(addr)
	caddr_t addr;
{
	register int c;
	struct pte savemap;

	savemap = mmap[0];
	*(int *)mmap = PG_V | PG_KR | btop(addr);
	mtpr(TBIS, vmmap);
	uncache(&vmmap[(int)addr & PGOFSET]);
	c = *(char *)&vmmap[(int)addr & PGOFSET];
	mmap[0] = savemap;
	mtpr(TBIS, vmmap);
	return (c & 0377);
}

putmemc(addr, val)
	caddr_t addr;
{
	struct pte savemap;

	savemap = mmap[0];
	*(int *)mmap = PG_V | PG_KW | btop(addr);
	mtpr(TBIS, vmmap);
	*(char *)&vmmap[(int)addr & PGOFSET] = val;

	mtpr(PADC, 0);
	mtpr(PACC, 0);

	mmap[0] = savemap;
	mtpr(TBIS, vmmap);
}
#endif

/*
 * Move pages from one kernel virtual address to another.
 * Both addresses are assumed to reside in the Sysmap,
 * and size must be a multiple of CLSIZE.
 */
pagemove(from, to, size)
	register caddr_t from, to;
	int size;
{
	register struct pte *fpte, *tpte;

	if (size % CLBYTES)
		panic("pagemove");
	fpte = kvtopte(from);
	tpte = kvtopte(to);
	while (size > 0) {
		*tpte++ = *fpte;
		*(int *)fpte++ = 0;
		mtpr(TBIS, from);
		mtpr(TBIS, to);
		mtpr(P1DC, to);		/* purge !! */
		from += NBPG;
		to += NBPG;
		size -= NBPG;
	}
}

/*
 * Code and data key management routines.
 *
 * The array ckey_cnt maintains the count of processes currently
 * sharing each code key.  The array ckey_cache maintains a record
 * of all code keys used since the last flush of the code cache.
 * Such keys may not be reused, even if unreferenced, until
 * the cache is flushed.  The data cache key handling is analogous.
 * The arrays ckey_cnt and ckey_cache are allways kept in such a way
 * that the following invariant holds:
 *	ckey_cnt > 0	=>'s	ckey_cache == 1
 * meaning as long as a code key is used by at least one process, it's
 * marked as being 'in the cache'. Of course, the following invariant
 * also holds:
 *	ckey_cache == 0	=>'s	ckey_cnt == 0
 * which is just the reciprocal of the 1'st invariant.
 * Equivalent invariants hold for the data key arrays.
 */
struct	keystats ckeystats = { NCKEY - 1 };
struct	keystats dkeystats = { NDKEY - 1 };

/* 
 * Release a code key.
 */
ckeyrelease(key)
	int key;
{
	register int s;

	s = spl8();
	if (--ckey_cnt[key] < 0) {
		printf("ckeyrelease: key = %d\n", key);
		ckey_cnt[key] = 0;
	}
	if (ckey_cnt[key] == 0)
		ckeystats.ks_dirty++;
	splx(s);
}

/* 
 * Release a data key.
 */
dkeyrelease(key)
	int key;
{
	register int s;

	s = spl8();
	if (--dkey_cnt[key] != 0) {
		printf("dkeyrelease: key = %d\n", key);
		dkey_cnt[key] = 0;
	}
	splx(s);	
	dkeystats.ks_dirty++;
}

/*
 * Invalidate the data cache for a process
 * by exchanging cache keys.
 */
dkeyinval(p)
	register struct proc *p;
{
	int s;

	dkeystats.ks_inval++;
	s = spl8();
	if (--dkey_cnt[p->p_dkey] != 0)
		dkey_cnt[p->p_dkey] = 0;
	if (p == u.u_procp && !noproc) {
		p->p_dkey = getdatakey();
		mtpr(DCK, p->p_dkey);
	} else
		p->p_dkey = 0;
	splx(s);	
}

/* 
 * Get a code key.
 * Strategy: try each of the following in turn
 * until a key is allocated.
 *
 * 1) Find an unreferenced key not yet in the cache.
 *    If this fails, a code cache purge will be necessary.
 * 2) Find an unreferenced key.  Mark all unreferenced keys
 *    as available and purge the cache.
 * 3) Free the keys from all processes not sharing keys.
 * 4) Free the keys from all processes.
 */
getcodekey()
{
	register int i, s, freekey;
	register struct proc *p;
	int desparate = 0;
	static int lastkey = MAXCKEY;

	ckeystats.ks_allocs++;
	s = spl8();
	freekey = 0;
	for (i = lastkey + 1; ; i++) {
		if (i > MAXCKEY)
			i = 1;
		if ((int)ckey_cache[i] == 0) {	/* free key, take it */
			ckey_cache[i] = 1, ckey_cnt[i] = 1;
			splx(s);
			ckeystats.ks_allocfree++;
			ckeystats.ks_avail--;
			lastkey = i;
			return (i);
		}
		if (ckey_cnt[i] == 0)		/* save for potential use */
			freekey = i;
		if (i == lastkey)
			break;
	}
	/*
	 * All code keys were marked as being in cache.
	 * If a key was in the cache, but not in use, grab it.
	 */
	if (freekey != 0) {
purge:
		/*
		 * If we've run out of free keys,
		 * try and free up some other keys to avoid
		 * future cache purges.
		 */
		ckey_cnt[freekey] = 1, ckey_cache[freekey] = 1;
		for (i = 1; i <= MAXCKEY; i++)
			if (ckey_cnt[i] == 0) {
				ckey_cache[i] = 0;
				ckeystats.ks_avail++;
			}
		mtpr(PACC, 0);
		splx(s);
		ckeystats.ks_dirty = 0;
		ckeystats.ks_norefs++;
		return (freekey);
	}

	/*
	 * All keys are marked as in the cache and in use.
	 * Release all unshared keys, or, on second pass,
	 * release all keys.
	 */
steal:
	for (p = allproc; p; p = p->p_nxt)
		if (p->p_ckey != 0 && (p->p_flag & SSYS) == 0) {
			i = p->p_ckey;
			if (ckey_cnt[i] == 1 || desparate) {
				p->p_ckey = 0;
				if (--ckey_cnt[i] == 0) {
					freekey = i;
					if (p->p_textp)
						p->p_textp->x_ckey = 0;
				}
			}
		}

	if (freekey) {
		ckeystats.ks_taken++;
		goto purge;
	} else {
		desparate++;
		goto steal;
	}
}

/* 
 * Get a data key.
 *
 * General strategy:
 * 1) Try to find a data key that isn't in the cache. Allocate it.
 * 2) If all data keys are in the cache, find one which isn't
 *    allocated.  Mark all unallocated keys as not in cache,
 *    purge the cache, and allocate this one.
 * 3) If all of them are allocated, free all process' keys
 *    and let them reclaim then as they run.
 */
getdatakey()
{
	register int i, freekey;
	register struct proc *p;
	int s;
	static int lastkey = MAXDKEY;

	dkeystats.ks_allocs++;
	s = spl8();
	freekey = 0;
	for (i = lastkey + 1; ; i++) {
		if (i > MAXDKEY)
			i = 1;
		if ((int)dkey_cache[i] == 0) {	/* free key, take it */
			dkey_cache[i] = 1, dkey_cnt[i] = 1;
			splx(s);
			dkeystats.ks_allocfree++;
			dkeystats.ks_avail--;
			lastkey = i;
			return (i);
		}
		if (dkey_cnt[i] == 0)
			freekey = i;
		if (i == lastkey)
			break;
	}
purge:
	if (freekey) {
		/*
		 * Try and free up some more keys to avoid
		 * future allocations causing a cache purge.
		 */
		dkey_cnt[freekey] = 1, dkey_cache[freekey] = 1;
		for (i = 1; i <= MAXDKEY; i++)
			if (dkey_cnt[i] == 0) {
				dkey_cache[i] = 0;
				dkeystats.ks_avail++;
			}
		mtpr(PADC, 0);
		splx(s);
		dkeystats.ks_norefs++;
		dkeystats.ks_dirty = 0;
		return (freekey);
	}

	/*
	 * Now, we have to take a key from someone.
	 * May as well take them all, so we get them
	 * from all of the idle procs.
	 */
	for (p = allproc; p; p = p->p_nxt)
		if (p->p_dkey != 0 && (p->p_flag & SSYS) == 0) {
			freekey = p->p_dkey;
			dkey_cnt[freekey] = 0;
			p->p_dkey = 0;
		}
	dkeystats.ks_taken++;
	goto purge;
}

/*VARGARGS1*/
vtoph(p, v)
	register struct proc *p;
	unsigned v;
{
	register struct pte *pte;
	register unsigned pg;

	pg = btop(v);
	if (pg >= BTOPKERNBASE)
		pte = &Sysmap[pg - BTOPKERNBASE];
	else
		pte = vtopte(p, pg);
	return ((pte->pg_pfnum << PGSHIFT) + (v & PGOFSET));
}
