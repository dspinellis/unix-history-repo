/*	vba.c	1.4	86/12/16	*/

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

/*
 * Tahoe VERSAbus adapator support routines.
 */

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
 *	1) The data length is not a multiple of sector size
 *	2) The base address + length cross a physical page boundary
 *	3) The virtual address for I/O is not in the system space.
 */

/*
 * I/O buffer preparation for possible buffered transfer.
 * The relevant page table entries are kept in the buf structure,
 * for later use by the driver's start or interrupt routine
 * when user's data has to be moved to the intermediate buffer.
 */
vbasetup(bp, sectsize)
	register struct buf *bp;
	int sectsize;	/* This disk's physical sector size */
{
	caddr_t	source_pte_adr;
	register int v;

	if ((((int)bp->b_un.b_addr & PGOFSET) + bp->b_bcount) > NBPG ||
	    (bp->b_bcount % sectsize) != 0 ||
	    ((int)bp->b_un.b_addr & 0xc0000000) != 0xc0000000) {
		bp->b_flags |= B_NOT1K;
		v = btop(bp->b_un.b_addr);
		source_pte_adr = (caddr_t)(bp->b_flags&B_DIRTY ?
		    vtopte(&proc[2], v) : vtopte(bp->b_proc, v));
		bp->b_ptecnt = (bp->b_bcount + NBPG -1 +
		    ((int)bp->b_un.b_addr & PGOFSET)) / NBPG;
		bcopy(source_pte_adr, (caddr_t)bp->b_upte,
		    (unsigned)bp->b_ptecnt*4);
	}
}

/*
 * This routine is usually called by the start routine. It
 * returns the physical address of the first byte for i/o, to
 * be presented to the controller. If intermediate buffering is
 * needed and a write out is done, now is the time to get the
 * original user's data in the buffer.
 */
vbastart(bp, v, map, utl)
	struct buf *bp;
	caddr_t v;		/* Driver's own intermediate buffer. */
	long *map;		/* A bunch of system pte's */
	caddr_t utl;	/* The system address mapped through 'map' */
{
	register phadr, i;

	if (bp->b_flags & B_NOT1K) {
		phadr = vtoph(bp->b_proc, (unsigned)v);
		if ((bp->b_flags & B_READ) == 0) {
			for (i = 0; i < bp->b_ptecnt; i++) {
				map[i] = bp->b_upte[i] 
					& ~PG_PROT | PG_V | PG_KR;
				mtpr(TBIS, utl + i*NBPG);
				mtpr(P1DC, utl + i*NBPG);
			}
			bcopy(((int)bp->b_un.b_addr & PGOFSET) + utl,
			    v, (unsigned)bp->b_bcount);
		}
	} else
		phadr = vtoph(bp->b_proc, (unsigned)bp->b_un.b_addr);
	return (phadr);
}

/*
 * Called by the driver's interrupt routine, after the data is
 * realy in or out. If that was a read, and the NOT1K flag was on,
 * now is the time to move the data back into user's space. 
 * Similar to the vbastart routine, but in the reverse direction.
 */
vbadone(bp, v, map, utl)
	register struct buf *bp;
	caddr_t v;	/* Driver's own intermediate buffer. */
	long *map;	/* A bunch of system pte's */
	caddr_t utl;	/* The system address mapped through 'map' */
{
	register i, cnt;

	if (bp->b_flags & B_READ)
		if (bp->b_flags & B_NOT1K) {
			for (cnt = bp->b_bcount; cnt >= 0; cnt -= NBPG) {
				mtpr(P1DC, (int)v + cnt-1);
				mtpr(P1DC, (caddr_t)bp->b_un.b_addr + cnt-1);
			}
			if (((int)v & PGOFSET) != 0)
				mtpr(P1DC, v);
			if (((int)bp->b_un.b_addr & PGOFSET) != 0)
				mtpr(P1DC, (caddr_t)bp->b_un.b_addr);
			for (i = 0; i < bp->b_ptecnt; i++) {
				map[i] = bp->b_upte[i] 
					& ~PG_PROT | PG_V | PG_KW;
				mtpr(TBIS, utl + i*NBPG);
			}
			bcopy(v, ((int)bp->b_un.b_addr & PGOFSET)+utl,
			    (unsigned)(bp->b_bcount - bp->b_resid));
		} else
			mtpr(P1DC, bp->b_un.b_addr);
	bp->b_flags &= ~B_NOT1K;
}
