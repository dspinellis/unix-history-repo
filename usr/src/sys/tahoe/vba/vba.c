/*	vba.c	1.1	85/07/21	*/

#include "../h/param.h"
#include "../h/buf.h"
#include "../h/cmap.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/dk.h"
#include "../h/map.h"
#include "../machine/mtpr.h"
#include "../machine/pte.h"
#include "../h/systm.h"
#include "../vba/vbavar.h"
#include "../h/user.h"
#include "../h/vmmac.h"
#include "../h/proc.h"


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

buf_setup(bp, sectsize)
struct	buf *bp;
long	sectsize;	/* This disk's physical sector size */
{
/*
 * IO buffer preparation for possible buffered transfer.
 * The relevant page table entries are kept in the 'buf' structure,
 * for later use by the driver's 'start' routine or 'interrupt'
 * routine, when user's data has to be moved to the intermediate
 * buffer.
 */
	caddr_t	source_pte_adr;

	if ((((int)bp->b_un.b_addr & PGOFSET) + bp->b_bcount) > NBPG ||
			(bp->b_bcount % sectsize) != 0 ||
			((int)bp->b_un.b_addr & 0xc0000000) != 0xc0000000) {
		bp->b_flags |= B_NOT1K;
		if (bp->b_flags & B_DIRTY)
			source_pte_adr = (caddr_t)vtopte(&proc[2], 
						btop(bp->b_un.b_addr));
			else source_pte_adr = (caddr_t)vtopte(bp->b_proc, 
						btop(bp->b_un.b_addr));
		bp->b_ptecnt = (bp->b_bcount + NBPG -1 +
			((int)bp->b_un.b_addr & PGOFSET)) / NBPG;
		bcopy (source_pte_adr, bp->b_upte, bp->b_ptecnt*4);
	}
}

int mapbusy;		/* semaphore on the system IOmap buffer */

get_ioadr(bp, buffer, map, utl)
struct buf *bp;
char	*buffer;	/* Driver's own intermediate buffer. */
long	*map;		/* A bunch of system pte's */
struct	user *utl;	/* The system address mapped through 'map' */
/*
 * This routine is usually called by the 'start' routine. It
 * returns the physical address of the first byte for IO, to
 * be presented to the controller. If intermediate buffering is
 * needed and a write out is done, now is the time to get the
 * original user's data in the buffer.
 */
{
	register phadr, i;

	if (bp->b_flags & B_NOT1K) {
		phadr = vtoph (bp->b_proc, buffer);
		if ( (bp->b_flags & B_READ) == 0) {
			for (i=0; i<bp->b_ptecnt; i++) {
				map[i] = bp->b_upte[i] 
					& ~PG_PROT | PG_V | PG_KR;
				mtpr ((caddr_t)utl + i*NBPG, TBIS);
				mtpr ((caddr_t)utl + i*NBPG, P1DC);
			}
			bcopy (((int)bp->b_un.b_addr & PGOFSET) + 
					(caddr_t)utl, buffer,bp->b_bcount);
		}
	} 
	else
		phadr = vtoph (bp->b_proc, bp->b_un.b_addr);
	return (phadr);
}

end_transfer(bp, buffer, map, utl)
register struct buf *bp;
char	*buffer;	/* Driver's own intermediate buffer. */
long	*map;	/* A bunch of system pte's */
struct	user *utl;	/* The system address mapped through 'map' */
{
/*
 * Called by the driver's interrupt routine, after the data is
 * realy in or out. If that was a read, and the NOT1K flag was on,
 * now is the time to move the data back into user's space. 
 * Mostly analogous to the get_ioadr routine, but in the reverse direction.
 */
	register i, cnt;

	if (bp->b_flags & B_READ)
		if (bp->b_flags & B_NOT1K) {
			for (cnt = bp->b_bcount ; cnt >= 0; cnt -= NBPG) {
				mtpr ((int)buffer + cnt-1, P1DC);
				mtpr ((caddr_t)bp->b_un.b_addr + cnt-1, P1DC);
			}
			if ( ((int)buffer & PGOFSET) != 0)
				mtpr (buffer, P1DC);
			if ( ((int)bp->b_un.b_addr & PGOFSET) != 0)
				mtpr ((caddr_t)bp->b_un.b_addr, P1DC);
			for (i=0; i<bp->b_ptecnt; i++) {
				map[i] = bp->b_upte[i] 
					& ~PG_PROT | PG_V | PG_KW;
				mtpr ((caddr_t)utl + i*NBPG, TBIS);
			}
			bcopy (buffer, 
				((int)bp->b_un.b_addr & PGOFSET) + 
					(caddr_t)utl, bp->b_bcount);
		}
		else
			mtpr (bp->b_un.b_addr, P1DC);
	bp->b_flags &= ~B_NOT1K;
}

movob (byte, address)
{
	asm(" movob 7(fp),*8(fp);");
}

movow (word, address)
{
	asm(" movow 6(fp),*8(fp);");
}

