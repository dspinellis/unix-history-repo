/*	vba.c	1.5	87/03/10	*/

#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "param.h"
#include "buf.h"
#include "cmap.h"
#include "conf.h"
#include "dir.h"
#include "dk.h"
#include "map.h"
#include "systm.h"
#include "user.h"
#include "vmparam.h"
#include "vmmac.h"
#include "proc.h"
#include "syslog.h"

#include "../tahoevba/vbavar.h"

#define	kvtopte(v) (&Sysmap[btop((int)(v) &~ KERNBASE)])
/*
 * Tahoe VERSAbus adapator support routines.
 */

vbainit(vb, xsize, flags)
	register struct vb_buf *vb;
	int xsize, flags;
{
	register struct pte *pte;
	register n;

	vb->vb_flags = flags;
	vbmapalloc(btoc(xsize)+1, &vb->vb_map, &vb->vb_utl);
	n = roundup(xsize, NBPG);
	if (vb->vb_rawbuf == 0)
		vb->vb_rawbuf = calloc(n);
	if ((int)vb->vb_rawbuf & PGOFSET)
		panic("vbinit");
	vb->vb_physbuf = vtoph((struct proc *)0, vb->vb_rawbuf);
	if (flags & VB_20BIT)
		vb->vb_maxphys = btoc(VB_MAXADDR20);
	else if (flags & VB_24BIT)
		vb->vb_maxphys = btoc(VB_MAXADDR24);
	else
		vb->vb_maxphys = btoc(VB_MAXADDR32);
	
	/*
	 * Make raw buffer pages uncacheable.
	 */
	pte = kvtopte(vb->vb_rawbuf);
	for (n = btoc(n); n--; pte++)
		pte->pg_nc = 1;
	mtpr(TBIA, 0);
}

/*
 * Next piece of logic takes care of unusual cases when less (or more) than
 * a full block (or sector) are required. This is done by the swaping
 * logic, when it brings page table pages from the swap device.
 * Since some controllers can't read less than a sector, the
 * only alternative is to read the disk to a temporary buffer and
 * then to move the amount needed back to the process (usually proc[0]
 * or proc[2]).
 * On Tahoe, the virtual addresses versus physical I/O problem creates
 * the need to move I/O data through an intermediate buffer whenever one
 * of the following is true:
 *	1) The data length is not a multiple of sector size (?)
 *	2) The buffer is not physically contiguous and the controller
 *	   does not support scatter-gather operations.
 *	3) The physical address for I/O is higher than addressible
 *	   by the device.
 */

/*
 * Check a transfer to see whether it can be done directly
 * to the destination buffer, or whether it must be copied.
 * If copying is necessary, the intermediate buffer is mapped.
 * This routine is called by the start routine. It
 * returns the physical address of the first byte for i/o, to
 * be presented to the controller. If intermediate buffering is
 * needed and a write out is done, now is the time to get the
 * original user's data in the buffer.
 */
u_long
vbasetup(bp, vb, sectsize)
	register struct buf *bp;
	register struct vb_buf *vb;
	int sectsize;
{
	register struct pte *spte, *dpte;
	register int p, i;
	int npf, o, v;

	o = (int)bp->b_un.b_addr & PGOFSET;
	npf = i = btoc(bp->b_bcount + o);
	vb->vb_iskernel = (((int)bp->b_un.b_addr & KERNBASE) == KERNBASE);
	if (vb->vb_iskernel)
		spte = kvtopte(bp->b_un.b_addr);
	else
		spte = vtopte((bp->b_flags&B_DIRTY) ? &proc[2] : bp->b_proc,
		    btop(bp->b_un.b_addr));
	if (bp->b_bcount % sectsize)
		goto copy;
	else if ((vb->vb_flags & VB_SCATTER) == 0 ||
	    vb->vb_maxphys != VB_MAXADDR32) {
		dpte = spte;
		for (p = (dpte++)->pg_pfnum; --i; dpte++) {
			if ((v = dpte->pg_pfnum) != p + NBPG &&
			    (vb->vb_flags & VB_SCATTER) == 0)
				goto copy;
			if (p >= vb->vb_maxphys)
				goto copy;
			p = v;
		}
		if (p >= vb->vb_maxphys)
			goto copy;
	}
	vb->vb_copy = 0;
	if ((bp->b_flags & BREAD) == 0)
		if (vb->vb_iskernel)
			vbastat.kw_raw++;
		else
			vbastat.uw_raw++;
	return ((spte->pg_pfnum << PGSHIFT) + o);

copy:
	vb->vb_copy = 1;
	if (vb->vb_iskernel) {
		if ((bp->b_flags & B_READ) == 0) {
			bcopy(bp->b_un.b_addr, vb->vb_rawbuf,
			    (unsigned)bp->b_bcount);
			vbastat.kw_copy++;
		}
	} else  {
		dpte = vb->vb_map;
		for (i = npf, p = (int)vb->vb_utl; i--; p += NBPG) {
			*(int *)dpte++ = (spte++)->pg_pfnum | PG_V | PG_KW;
			mtpr(TBIS, p);
		}
		if ((bp->b_flags & B_READ) == 0) {
			bcopy(vb->vb_utl + o, vb->vb_rawbuf,
			    (unsigned)bp->b_bcount);
			vbastat.uw_copy++;
		}
	}
	return (vb->vb_physbuf);
}

/*
 * Called by the driver's interrupt routine, after DMA is completed.
 * If the operation was a read, copy data to final buffer if necessary
 * or invalidate data cache for cacheable direct buffers.
 * Similar to the vbastart routine, but in the reverse direction.
 */
vbadone(bp, vb)
	register struct buf *bp;
	register struct vb_buf *vb;
{
	register npf;
	register caddr_t v;
	int o;

	if (bp->b_flags & B_READ) {
		o = (int)bp->b_un.b_addr & PGOFSET;
		if (vb->vb_copy) {
			if (vb->vb_iskernel) {
				bcopy(vb->vb_rawbuf, bp->b_un.b_addr,
				    (unsigned)(bp->b_bcount - bp->b_resid));
				vbastat.kr_copy++;
			} else {
				bcopy(vb->vb_rawbuf, vb->vb_utl + o,
				    (unsigned)(bp->b_bcount - bp->b_resid));
				dkeyinval(bp->b_proc);
				vbastat.ur_copy++;
			}
		} else {
			if (vb->vb_iskernel) {
				npf = btoc(bp->b_bcount + o);
				for (v = bp->b_un.b_addr; npf--; v += NBPG)
					mtpr(P1DC, (int)v);
				vbastat.kr_raw++;
			} else {
				dkeyinval(bp->b_proc);
				vbastat.ur_raw++;
			}
		}
	}
}
