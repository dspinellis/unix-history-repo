/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm_subr.c	7.3 (Berkeley) 4/2/87
 */

#include "../machine/pte.h"
#include "../machine/mtpr.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "vm.h"
#include "proc.h"
#include "cmap.h"
#include "inode.h"
#include "buf.h"
#include "text.h"
#include "fs.h"

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
		mapin(ppte, (u_int)vaddr, ppte->pg_pfnum, (int)(PG_V|PG_KW));
#if defined(tahoe)
		mtpr(P1DC, vaddr);
#endif
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
	struct pte *pte;
{
	register int j;

	j = pte - p->p_p0br;
	if (j < p->p_tsize + p->p_dsize)
		return (j);
	return ((BTOPUSRSTACK + UPAGES) - p->p_szpt * NPTEPG + j);
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
				pte->pg_blkno = bn + btodb(i * CLBYTES);
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
