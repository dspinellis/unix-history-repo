/*	vm_subr.c	4.15	82/12/17	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/vm.h"
#include "../h/proc.h"
#include "../h/cmap.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/text.h"
#include "../h/fs.h"

#ifdef vax
#include "../vax/mtpr.h"
#endif

/*
 * Make uarea of process p addressible at kernel virtual
 * address uarea through sysmap locations starting at map.
 */
uaccess(p, map, uarea)
	register struct proc *p;
	struct pte *map;
	register struct user *uarea;
{
	register int i;
	register struct pte *mp = map;

	for (i = 0; i < UPAGES; i++) {
		*(int *)mp = 0;
		mp->pg_pfnum = p->p_addr[i].pg_pfnum;
		mp++;
	}
	vmaccess(map, (caddr_t)uarea, UPAGES);
}

/*
 * Validate the kernel map for size ptes which
 * start at ppte in the sysmap, and which map
 * kernel virtual addresses starting with vaddr.
 */
vmaccess(ppte0, vaddr, size0)
	struct pte *ppte0;
	register caddr_t vaddr;
	int size0;
{
	register struct pte *ppte = ppte0;
	register int size = size0;

	while (size != 0) {
		mapin(ppte, btop(vaddr), (int)(*(int *)ppte & PG_PFNUM), 1,
			(int)(PG_V|PG_KW));
		ppte++;
		vaddr += NBPG;
		--size;
	}
}

/* 
 * Convert a pte pointer to
 * a virtual page number.
 */
ptetov(p, pte)
	register struct proc *p;
	register struct pte *pte;
{

	if (isatpte(p, pte))
		return (tptov(p, ptetotp(p, pte)));
	else if (isadpte(p, pte))
		return (dptov(p, ptetodp(p, pte)));
	else
		return (sptov(p, ptetosp(p, pte)));
}

/*
 * Convert a virtual page 
 * number to a pte address.
 */
struct pte *
vtopte(p, v)
	register struct proc *p;
	register unsigned v;
{

	if (isatsv(p, v))
		return (tptopte(p, vtotp(p, v)));
	else if (isadsv(p, v))
		return (dptopte(p, vtodp(p, v)));
	else 
		return (sptopte(p, vtosp(p, v)));
}

/*
 * Initialize the page tables for paging from an inode,
 * by scouring up the indirect blocks in order.
 * Corresponding area of memory should have been vmemfree()d
 * first or just created.
 */
vinifod(pte, fileno, ip, bfirst, count)
	register struct fpte *pte;
	int fileno;
	register struct inode *ip;
	daddr_t bfirst;
	size_t count;
{
	int blast = bfirst + howmany(count, CLSIZE);
	register int i, j;
	int bn;
	register struct fs *fs = ip->i_fs;
	int nclpbsize = fs->fs_bsize / CLBYTES;

	while (bfirst < blast) {
		i = bfirst % nclpbsize;
		bn = fsbtodb(fs, bmap(ip, bfirst / nclpbsize, B_READ, 0));
		for ( ; i < nclpbsize; i++) {
			pte->pg_fod = 1;
			pte->pg_fileno = fileno;
			if (u.u_error || bn < 0) {
				pte->pg_blkno = 0;
				pte->pg_fileno = PG_FZERO;
				cnt.v_nzfod += CLSIZE;
			} else {
				pte->pg_blkno =
				    bn + (i * CLBYTES / DEV_BSIZE);
				cnt.v_nexfod += CLSIZE;
			}
			for (j = 1; j < CLSIZE; j++)
				pte[j] = pte[0];
			pte += CLSIZE;
			bfirst++;
			if (bfirst == blast)
				break;
		}
	}
}
