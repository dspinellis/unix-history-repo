/*	vm_machdep.c	1.7	86/12/15	*/

#include "../machine/pte.h"

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

#include "../tahoe/cpu.h"
#include "../tahoe/mtpr.h"

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

#ifndef mapin
mapin(pte, v, pfnum, count, prot)
	struct pte *pte;
	u_int v, pfnum;
	int count, prot;
{

	while (count > 0) {
		*(int *)pte++ = pfnum | prot;
		mtpr(TBIS, ptob(v));
		v++;
		pfnum++;
		count--;
	}
}
#endif

#ifdef notdef
/*ARGSUSED*/
mapout(pte, size)
	register struct pte *pte;
	int size;
{

	panic("mapout");
}
#endif

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
	fpte = &Sysmap[btop(from - 0xC0000000)];
	tpte = &Sysmap[btop(to - 0xC0000000)];
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

#ifndef vtopte
/*
 * Convert a virtual page 
 * number to a pte address.
 */
/*VARARGS1*/
struct pte *
vtopte(p, v)
	register struct proc *p;
{

	if ((v & 0x300000) == 0x300000)
		return (struct pte *)(mfpr(SBR) + 0xc0000000 + (v&0xfffff)*4);
	if (p == 0) {
		printf("vtopte v %x\n", v);		/* XXX */
		panic("vtopte (no proc)");
	}
	if (v < p->p_tsize + p->p_dsize)
		return (p->p_p0br + v);
	return (p->p_addr + (v - BTOPUSRSTACK));
}
#endif

/*
 * Code and data key management routines.
 *
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
}

struct	keystats ckeystats;
struct	keystats dkeystats;
/* 
 * Get a code key.
 */
getcodekey()
{
	register int i, s, freekey, sharedkey;
	register struct proc *p;

	ckeystats.ks_allocs++;
	s = spl8();
	freekey = 0;
	for (i = 1; i <= MAXCKEY; i++) {
		if ((int)ckey_cache[i] == 0) {	/* free key, take it */
			ckey_cache[i] = 1, ckey_cnt[i] = 1;
			splx(s);
			ckeystats.ks_free++;
			return (i);
		}
		if (ckey_cnt[i] == 0) {		/* save for potential use */
			if (freekey == 0)
				freekey = i;
		} else if (ckey_cnt[i] > 1 && i != MAXCKEY)
			sharedkey = i;
	}
	/*
	 * All code keys were marked as being in cache.
	 * Moreover, we are assured that sharedkey has a meaningful value,
	 * since we know that the init process and the shell are around
	 * and they have shared text!
	 */
	/*
	 * If a key was in the cache, but not in use, grab it.
	 */
	if (freekey != 0) {
		/*
		 * If we've run out of bonified free keys,
		 * try and free up some other keys to avoid
		 * future cache purges.
		 */
		for (i = 1; i <= MAXCKEY; i++)
			if (ckey_cnt[i] == 0)
				ckey_cache[i] = 0;
		ckey_cnt[freekey] = 1, ckey_cache[freekey] = 1;
		mtpr(PACC, 0);
		splx(s);
		ckeystats.ks_norefs++;
		return (freekey);
	}

	/*
	 * All keys are marked as in the cache and in use.
	 *
	 * Strip some process of their code key. First time,
	 * 1) Try hard not to do that to kernel processes !!
	 * 2) Try hard NOT to strip shared text processes of
	 *    their (shared) key, because then they'll run
	 *    with different keys from now on, i.e. less efficient
	 *    cache utilization.
	 */
	for (p = proc; p < procNPROC; p++)
		/*
		 * Look for a meaningful key but not
		 * used and not shared text.
		 */
		if (p->p_ckey && p->p_ckey != MAXCKEY &&
		    ckey_cnt[p->p_ckey] < 2) {
			i = p->p_ckey;
			p->p_ckey = 0;
			ckey_cnt[i] = 1, ckey_cache[i] = 1;
			mtpr(PACC, 0);
			splx(s);
			ckeystats.ks_taken++;
			return (i);
		}

	/*
	 * Second time around!
	 * Highly unlikely situation. It means that all keys are
	 * allocated AND shared (i.e. we have at least 510 active
	 * processes).
	 * Strip some of them. We pick some key (known to be shared
	 * by several processes) and strip the poor process group.
	 * At least 2 processes will loose but we gain one key to be reused.
	 * The way 'shared_key' was produced (above) virtually assures
	 * us that this key isn't the 'init' group key (1) nor the
	 * 'shell' group key (2 or 3). It's probably something like 254.
	 * Could be more straightforward to strip all processes, but it's
	 * better to invest in one more loop here and keep the cache
	 * utilization to a maximum.
	 */
	for (p = proc; p < procNPROC; p++)
		if (p->p_ckey == sharedkey) {
			p->p_ckey = 0;
			ckey_cnt[sharedkey]--;
		}
	if (ckey_cnt[sharedkey] != 0) {
		printf("getcodekey: key = %d cnt = %d\n",
		    sharedkey, ckey_cnt[sharedkey]);
		panic("getcodekey");
	}
	ckey_cnt[sharedkey] = 1, ckey_cache[sharedkey] = 1;
	mtpr(PACC, 0);
	splx(s);
	ckeystats.ks_shared++;
	return (sharedkey);
}

/* 
 * Get a data key.
 *
 * General strategy:
 * 1) Try to find a data key that isn't in the cache. Allocate it.
 * 2) If all data keys are in the cache, find one which isn't
 *    allocated. Clear all status and allocate this one.
 * 3) If all of them are allocated, pick some process, strip him
 *    of the data key and allocate it. We (cold-bloodedly) pick
 *    one process to be the poor looser because that's the
 *    easiest way to do it and because this extreme situation
 *    ( >255 active processes ) is expected to be temporary,
 *    after which 1) or 2) above should be the usual case.
 * The poor looser is the first process which has a data key.
 * However, we try to spare known kernel processes and daemons
 * (fired at bootstrap time), by searching from proc[LOOSER] and on.
 */
getdatakey()
{
	register int i, s, freekey;
	register struct proc *p;

	dkeystats.ks_allocs++;
	s = spl8();
	freekey = 0;
	for (i = 1; i <= MAXDKEY; i++) {
		if ((int)dkey_cache[i] == 0) {	/* free key, take it */
			dkey_cache[i] = 1, dkey_cnt[i] = 1;
			splx(s);
			dkeystats.ks_free++;
			return (i);
		}
		if (dkey_cnt[i] == 0 && freekey == 0)
			freekey = i;
	}
	if (freekey) {
		/*
		 * Try and free up some more keys to avoid
		 * future allocations causing a cache purge.
		 */
		for (i = 1; i < MAXDKEY; i++)
			if (dkey_cnt[i] == 0)
				dkey_cache[i] = 0;
		dkey_cnt[freekey] = 1, dkey_cache[freekey] = 1;
		mtpr(PADC, 0);
		splx(s);
		dkeystats.ks_norefs++;
		return (freekey);
	}

	/*
	 * Now, we have to take a code from someone.
	 */
#define LOOSER 20
	for (p = &proc[LOOSER]; p < procNPROC; p++)
		if (p->p_dkey != 0) {
			i = p->p_dkey;
			p->p_dkey = 0;
			dkey_cnt[i] = 1;
			dkey_cache[i] = 1;
			mtpr(PADC, 0);
			splx(s);
			dkeystats.ks_taken++;
			return (i);
		}
	panic("getdatakey");
	/*NOTREACHED*/
}

/*VARGARGS1*/
vtoph(p, v)
	register struct proc *p;
	register unsigned v;
{
	register struct pte *pte;

	pte = vtopte(p, btop(v));
	return ((pte->pg_pfnum << PGSHIFT) + (v & PGOFSET));
}
