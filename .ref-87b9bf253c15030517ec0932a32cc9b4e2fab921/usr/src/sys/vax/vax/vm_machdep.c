/*	vm_machdep.c	5.3	82/12/17	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/cmap.h"
#include "../h/mount.h"
#include "../h/vm.h"

#include "../vax/mtpr.h"

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
		mtpr(TBIS, vaddr + sizeof (struct user));
}

/*
 *
 */
mapin(pte, v, pfnum, count, prot)
	struct pte *pte;
	u_int v;
	int pfnum, count, prot;
{

	while (count > 0) {
		*(int *)pte++ = pfnum | prot;
		mtpr(TBIS, ptob(v));
		v++;
		pfnum++;
		count--;
	}
}

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
 * Check that a process will not be too large.
 */
chksize(ts, ds, ss)
	size_t ts, ds, ss;
{

	if (ts>MAXTSIZ || ds>MAXDSIZ || ss>MAXSSIZ) {
		u.u_error = ENOMEM;
		return(1);
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
			munhash(mount[c->c_mdev].m_dev, (daddr_t)c->c_blkno);
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
	mmap[0] = savemap;
	mtpr(TBIS, vmmap);
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
	fpte = &Sysmap[btop(from - 0x80000000)];
	tpte = &Sysmap[btop(to - 0x80000000)];
	while (size > 0) {
		*tpte++ = *fpte;
		*(int *)fpte++ = 0;
		mtpr(TBIS, from);
		mtpr(TBIS, to);
		from += NBPG;
		to += NBPG;
		size -= NBPG;
	}
}
