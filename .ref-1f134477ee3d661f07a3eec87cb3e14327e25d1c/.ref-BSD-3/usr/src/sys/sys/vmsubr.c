/*	vmsubr.c	2.1	1/5/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/vm.h"
#include "../h/proc.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/text.h"

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
		mp->pg_pfnum = p->p_addr[i];
		mp++;
	}
	vmaccess(map, (caddr_t)uarea, UPAGES);
}

/*
 * Validate the kernel map for size ptes which
 * start at ppte in the sysmap, and which map
 * kernel virtual addresses starting with vaddr.
 */
vmaccess(ppte, vaddr, size)
	register struct pte *ppte;
	register caddr_t vaddr;
	register int size;
{

	while (size != 0) {
		*(int *)ppte++ |= PG_V|PG_KW;
		mtpr(TBIS, vaddr);
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

struct	buf *vbmap();
/*
 * Initialize the page tables for paging from an inode,
 * by scouring up the indirect blocks in order.
 * Corresponding area of memory should have been vmemfree()d
 * first or just created.
 */
vinifod(pte, fileno, ip, bstart, count)
	register struct fpte *pte;
	int fileno;
	register struct inode *ip;
	daddr_t bstart;
	int count;
{
	register int i, j;
	struct buf *bp;
	int indx;
	register daddr_t *pp;

	while (count > 0) {
		if (bstart < NADDR - 3) {
			((struct pte *)pte)->pg_vreadm = 0;
			pte->pg_fod = 1;
			pte->pg_fileno = fileno;
			pte->pg_blkno = ip->i_un.i_addr[bstart];
			if (pte->pg_blkno == 0) {
				pte->pg_fileno = PG_FZERO;
				pte->pg_blkno = 0;
				cnt.v_nzfod += CLSIZE;
			} else if (fileno == PG_FTEXT)
				cnt.v_nexfod += CLSIZE;
			else {
				cnt.v_nvrfod += CLSIZE;
				u.u_vrpages[fileno] += CLSIZE;
			}
			for (j = 1; j < CLSIZE; j++)
				pte[j] = pte[0];
			pte += CLSIZE;
			bstart++;
			count -= CLSIZE;
		} else {
			mtpr(TBIA, 1);		/* conservative */
			bp = vbmap(ip, bstart);
			indx = (bstart - (NADDR - 3)) % NINDIR;
			i = imin((NINDIR - indx) * CLSIZE, count);
			bstart += i / CLSIZE;
			count -= i;
			if (bp) {
				if (fileno == PG_FTEXT)
					cnt.v_nexfod += i;
				else {
					cnt.v_nvrfod += i;
					u.u_vrpages[fileno] += i;
				}
				pp = &bp->b_un.b_daddr[indx];
				do {
					((struct pte *)pte)->pg_vreadm = 0;
					pte->pg_fod = 1;
					pte->pg_blkno = *pp++;
					if (pte->pg_blkno)
						pte->pg_fileno = fileno;
					else {
						pte->pg_fileno = PG_FZERO;
						pte->pg_blkno = 0;
						u.u_vrpages[fileno] -= CLSIZE;
						cnt.v_nvrfod -= CLSIZE;
						cnt.v_nzfod += CLSIZE;
					}
					for (j = 1; j < CLSIZE; j++)
						pte[j] = pte[0];
					pte += CLSIZE;
				} while ((i -= CLSIZE) > 0);
				brelse(bp);
			} else {
				cnt.v_nzfod += i;
				do {
					((struct pte *)pte)->pg_vreadm = 0;
					pte->pg_fod = 1;
					pte->pg_fileno = PG_FZERO;
					distcl(pte);
					pte += CLSIZE;
				} while ((i -= CLSIZE) > 0);
			}
		}
	}
	mtpr(TBIA, 1);		/* necessary! */
}

/*
 * Vbmap returns a block full of indirect pointers for a given block offset
 * in a file.  It returns 0 if a missing address block was encountered,
 * in which case the pages can be normal zfod pages.
 */
struct buf *
vbmap(ip, bn)
register struct inode *ip;
daddr_t bn;
{
	register i;
	struct buf *bp;
	int j, sh;
	daddr_t nb;
	dev_t dev = ip->i_dev;

	if (bn < NADDR-3)
		panic("vbmap");

	/*
	 * addresses NADDR-3, NADDR-2, and NADDR-1
	 * have single, double, triple indirect blocks.
	 * the first step is to determine
	 * how many levels of indirection.
	 */
	sh = 0;
	nb = 1;
	bn -= NADDR-3;
	for (j = 3; j > 0; j--) {
		sh += NSHIFT;
		nb <<= NSHIFT;
		if(bn < nb)
			break;
		bn -= nb;
	}
	if (j == 0)
		goto noblk;

	/*
	 * fetch the address from the inode
	 */
	nb = ip->i_un.i_addr[NADDR-j];

	/*
	 * fetch through the indirect blocks
	 */
	for (;;) {
		if (nb == 0)
			return ((daddr_t)0);
		bp = bread(dev, nb);
		if (bp->b_flags & B_ERROR) {
			brelse(bp);
			goto noblk;
		}
		if (j == 3)
			break;
		j++;
		sh -= NSHIFT;
		i = (bn>>sh) & NMASK;
		nb = bp->b_un.b_daddr[i];
		brelse(bp);
		if (nb == 0)
			goto noblk;
	}
	return (bp);

noblk:
	return ((struct buf *)0);
}
