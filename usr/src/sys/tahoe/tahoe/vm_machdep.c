/*	vm_machdep.c	1.1	85/07/21	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/cmap.h"
#include "../h/mount.h"
#include "../h/vm.h"
#include "../h/text.h"

#include "../machine/mtpr.h"

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
		mtpr(vaddr + sizeof (struct user) + NBPG - 1, TBIS);
}

#ifndef mapin
mapin(pte, v, pfnum, count, prot)
	struct pte *pte;
	u_int v, pfnum;
	int count, prot;
{

	while (count > 0) {
		*(int *)pte++ = pfnum | prot;
		mtpr(ptob(v), TBIS);
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
 */
chksize(ts, ds, ss)
	register unsigned ts, ds, ss;
{
	static int maxdmap = 0;

	if (ts > MAXTSIZ || ds > MAXDSIZ || ss > MAXSSIZ) {
		u.u_error = ENOMEM;
		return (1);
	}
	/* check for swap map overflow */
	if (maxdmap == 0) {
		register int i, blk;

		blk = dmmin;
		for (i = 0; i < NDMAP; i++) {
			maxdmap += blk;
			if (blk < dmmax)
				blk *= 2;
		}
	}
	if (ctod(ts) > NXDAD * dmtext ||
	    ctod(ds) > maxdmap || ctod(ss) > maxdmap) {
		u.u_error = ENOMEM;
		return (1);
	}
	/*
	 * Make sure the process isn't bigger than our
	 * virtual memory limit.
	 *
	 * THERE SHOULD BE A CONSTANT FOR THIS.
	 */
	if (ts + ds + ss + LOWPAGES + HIGHPAGES > btoc(USRSTACK)) {
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
		mtpr(0, TBIA);
		return;
	}
	while (size > 0) {
		mtpr(a, TBIS);
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
	mtpr(0, TBIA);
}

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
	mtpr(vmmap, TBIS);
	uncache (&vmmap[(int)addr & PGOFSET]);
	c = *(char *)&vmmap[(int)addr & PGOFSET];
	mmap[0] = savemap;
	mtpr(vmmap, TBIS);
	return (c & 0377);
}

putmemc(addr, val)
	caddr_t addr;
{
	struct pte savemap;

	savemap = mmap[0];
	*(int *)mmap = PG_V | PG_KW | btop(addr);
	mtpr(vmmap, TBIS);
	*(char *)&vmmap[(int)addr & PGOFSET] = val;

	mtpr (0, PADC);
	mtpr (0, PACC);

	mmap[0] = savemap;
	mtpr(vmmap, TBIS);
}

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
		mtpr(from, TBIS);
		mtpr(to, TBIS);
		mtpr(to, P1DC);		/* purge !! */
		from += NBPG;
		to += NBPG;
		size -= NBPG;
	}
}

#ifndef GLOBKEY
ckeyrelease(key)
/* 
 * release code key
 */
{
	register int ipl,i,j ;
	ipl = spl8();
	if (--ckey_cnt[key-1] < 0 ) {
/*
		panic ("Code key release");
*/
		printf("Ckey release, key=%d\n", key);
		ckey_cnt[key-1] = 0;
		splx(ipl);
	}
	splx(ipl);
}


dkeyrelease(key)
/* 
 * release data key
 */
{
	if (--dkey_cnt[key-1] != 0 ) panic ("Data key release");
}


int
getcodekey()
/* 
 * Get a code key
 */
{
	register int i, ipl, first;

	first = 1;
	ipl = spl8();
retry:
	for (i=0; i<255; i++) {
		if ( (int)ckey_cache[i] == 0) {
			ckey_cache[i] = 1;
			ckey_cnt[i] = 1;
			splx(ipl);
			return (i+1);
		};
	}
	if ( !first) {
		splx(ipl);
		panic ("Not enough code keys\n");
	}
	mtpr (0, PACC);
	first = 0;
	for (i=0; i<255; i++)
		if ( ckey_cnt[i] > 0 ) ckey_cache[i] = 1;
		else ckey_cache[i] = 0;
	goto retry;
}

int
getdatakey()
/* 
 * Get a data key
 */
{
	register int i, ipl, first;
	
	first = 1;
	ipl = spl8();
retry:
	for (i=0; i<255; i++)
		if ( (int)dkey_cache[i] == 0) {
			dkey_cache[i] = 1;
			dkey_cnt[i] = 1;
			splx(ipl);
			return (i+1);
		};
	if ( !first) {
		splx(ipl);
		panic("Not enough data keys\n");
	}
	mtpr (0, PADC);
	first = 0;
	for (i=0; i<255; i++)
		if ( dkey_cnt[i] > 0 ) dkey_cache[i] = 1;
		else dkey_cache[i] = 0;
	goto retry;
}
#endif

/* General (includes system) virtual address to physical */
vtoph(p, v)
register struct proc *p;
register unsigned v;
{
	register struct pte *thispte;

	thispte = vtopte (p, btop(v));
	return ( (thispte->pg_pfnum << PGSHIFT) + (v & PGOFSET));
}
		
