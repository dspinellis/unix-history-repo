/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
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
 *	@(#)vm_subr.c	7.14 (Berkeley) 6/30/90
 */

#include "param.h"
#include "user.h"
#include "vm.h"
#include "proc.h"
#include "vnode.h"
#include "mount.h"
#include "file.h"
#include "buf.h"

#include "machine/pte.h"
#include "machine/mtpr.h"

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
#if defined(hp300)
	DCIS();
#endif
#if defined(i386)
	tlbflush();
#endif
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
	if (j < p->p_tsize + p->p_dsize + p->p_mmsize)
		return (j);
	return ((BTOPUSRSTACK + HIGHPAGES) - p->p_szpt * NPTEPG + j);
}

/*
 * Initialize the page tables for paging from an inode,
 * by scouring up the indirect blocks in order.
 * Corresponding area of memory should have been vmemfree()d
 * first or just created.
 */
vinifod(p, pte, fileno, vp, bfirst, count)
	struct proc *p;
	register struct fpte *pte;
	int fileno;
	register struct vnode *vp;
	daddr_t bfirst;
	segsz_t count;
{
	int blast = bfirst + howmany(count, CLSIZE);
	register int i, j;
	int bn;
	int nclpbsize = vp->v_mount->mnt_stat.f_bsize / CLBYTES;

	/*
	 * Blocks of an executable may still be in the buffer
	 * cache, so we explicitly flush them out to disk
	 * so that the proper data will be paged in.
	 */
	vflushbuf(vp, B_SYNC);
	/*
	 * Map in the appropriate block numbers for each page
	 * of the executable.
	 */
	while (bfirst < blast) {
		i = bfirst % nclpbsize;
		if (VOP_BMAP(vp, bfirst / nclpbsize, (struct vnode **)0, &bn)) {
			swkill(p, "I/O error mapping pages");
			return;
		}
		for ( ; i < nclpbsize; i++) {
			pte->pg_fod = 1;
			pte->pg_fileno = fileno;
			if (bn < 0) {
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
