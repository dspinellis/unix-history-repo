#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/page.h"

#define	DISKMON	1

#ifdef	DISKMON
struct {
	int	nbuf;
	long	nread;
	long	nreada;
	long	ncache;
	long	nwrite;
	long	bufcount[NBUF];
} io_info;
#endif

/*
 * swap IO headers.
 * they are filled in to point
 * at the desired IO operation.
 */
struct	buf	swbuf1;
struct	buf	swbuf2;

/*
 * The following several routines allocate and free
 * buffers with various side effects.  In general the
 * arguments to an allocate routine are a device and
 * a block number, and the value is a pointer to
 * to the buffer header; the buffer is marked "busy"
 * so that no one else can touch it.  If the block was
 * already in core, no I/O need be done; if it is
 * already busy, the process waits until it becomes free.
 * The following routines allocate a buffer:
 *	getblk
 *	bread
 *	breada
 * Eventually the buffer must be released, possibly with the
 * side effect of writing it out, by using one of
 *	bwrite
 *	bdwrite
 *	bawrite
 *	brelse
 */

/*
 * Read in (if necessary) the block and return a buffer pointer.
 */
struct buf *
bread(dev, blkno)
dev_t dev;
daddr_t blkno;
{
	register struct buf *bp;

	bp = getblk(dev, blkno);
	if (bp->b_flags&B_DONE) {
#ifdef	DISKMON
		io_info.ncache++;
#endif
		return(bp);
	}
	bp->b_flags |= B_READ;
	bp->b_bcount = BSIZE;
	(*bdevsw[major(dev)].d_strategy)(bp);
#ifdef	DISKMON
	io_info.nread++;
#endif
	iowait(bp);
	return(bp);
}

/*
 * Read in the block, like bread, but also start I/O on the
 * read-ahead block (which is not allocated to the caller)
 */
struct buf *
breada(dev, blkno, rablkno)
dev_t dev;
daddr_t blkno, rablkno;
{
	register struct buf *bp, *rabp;

	bp = NULL;
	if (!incore(dev, blkno)) {
		bp = getblk(dev, blkno);
		if ((bp->b_flags&B_DONE) == 0) {
			bp->b_flags |= B_READ;
			bp->b_bcount = BSIZE;
			(*bdevsw[major(dev)].d_strategy)(bp);
#ifdef	DISKMON
			io_info.nread++;
#endif
		}
	}
	if (rablkno && !incore(dev, rablkno)) {
		rabp = getblk(dev, rablkno);
		if (rabp->b_flags & B_DONE)
			brelse(rabp);
		else {
			rabp->b_flags |= B_READ|B_ASYNC;
			rabp->b_bcount = BSIZE;
			(*bdevsw[major(dev)].d_strategy)(rabp);
#ifdef	DISKMON
			io_info.nreada++;
#endif
		}
	}
	if(bp == NULL)
		return(bread(dev, blkno));
	iowait(bp);
	return(bp);
}

/*
 * Write the buffer, waiting for completion.
 * Then release the buffer.
 */
bwrite(bp)
register struct buf *bp;
{
	register flag;

	flag = bp->b_flags;
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI | B_AGE);
	bp->b_bcount = BSIZE;
#ifdef	DISKMON
	io_info.nwrite++;
#endif
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);
	if ((flag&B_ASYNC) == 0) {
		iowait(bp);
		brelse(bp);
	} else if (flag & B_DELWRI)
		bp->b_flags |= B_AGE;
	else
		geterror(bp);
}

/*
 * Release the buffer, marking it so that if it is grabbed
 * for another purpose it will be written out before being
 * given up (e.g. when writing a partial block where it is
 * assumed that another write for the same block will soon follow).
 * This can't be done for magtape, since writes must be done
 * in the same order as requested.
 */
bdwrite(bp)
register struct buf *bp;
{
	register struct buf *dp;

	dp = bdevsw[major(bp->b_dev)].d_tab;
	if(dp->b_flags & B_TAPE)
		bawrite(bp);
	else {
		bp->b_flags |= B_DELWRI | B_DONE;
		brelse(bp);
	}
}

/*
 * Release the buffer, start I/O on it, but don't wait for completion.
 */
bawrite(bp)
register struct buf *bp;
{

	bp->b_flags |= B_ASYNC;
	bwrite(bp);
}

/*
 * release the buffer, with no I/O implied.
 */
brelse(bp)
register struct buf *bp;
{
	register struct buf **backp;
	register s;

	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	if (bfreelist.b_flags&B_WANTED) {
		bfreelist.b_flags &= ~B_WANTED;
		wakeup((caddr_t)&bfreelist);
	}
	if (bp->b_flags&B_ERROR)
		bp->b_dev = NODEV;  /* no assoc. on error */
	s = spl6();
	if(bp->b_flags & B_AGE) {
		backp = &bfreelist.av_forw;
		(*backp)->av_back = bp;
		bp->av_forw = *backp;
		*backp = bp;
		bp->av_back = &bfreelist;
	} else {
		backp = &bfreelist.av_back;
		(*backp)->av_forw = bp;
		bp->av_back = *backp;
		*backp = bp;
		bp->av_forw = &bfreelist;
	}
	bp->b_flags &= ~(B_WANTED|B_BUSY|B_ASYNC|B_AGE);
	splx(s);
}

/*
 * See if the block is associated with some buffer
 * (mainly to avoid getting hung up on a wait in breada)
 */
incore(dev, blkno)
dev_t dev;
daddr_t blkno;
{
	register struct buf *bp;
	register struct buf *dp;

	dp = bdevsw[major(dev)].d_tab;
	for (bp=dp->b_forw; bp != dp; bp = bp->b_forw)
		if (bp->b_blkno==blkno && bp->b_dev==dev)
			return(1);
	return(0);
}

/*
 * Assign a buffer for the given block.  If the appropriate
 * block is already associated, return it; otherwise search
 * for the oldest non-busy buffer and reassign it.
 */
struct buf *
getblk(dev, blkno)
dev_t dev;
daddr_t blkno;
{
	register struct buf *bp;
	register struct buf *dp;
#ifdef	DISKMON
	register i;
#endif

	if(major(dev) >= nblkdev)
		panic("blkdev");

    loop:
	spl0();
	dp = bdevsw[major(dev)].d_tab;
	if(dp == NULL)
		panic("devtab");
	for (bp=dp->b_forw; bp != dp; bp = bp->b_forw) {
		if (bp->b_blkno!=blkno || bp->b_dev!=dev)
			continue;
		spl6();
		if (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
			goto loop;
		}
		spl0();
#ifdef	DISKMON
		i = 0;
		dp = bp->av_forw;
		while (dp != &bfreelist) {
			i++;
			dp = dp->av_forw;
		}
		if (i<NBUF)
			io_info.bufcount[i]++;
#endif
		notavail(bp);
		return(bp);
	}
	spl6();
	if (bfreelist.av_forw == &bfreelist) {
		bfreelist.b_flags |= B_WANTED;
		sleep((caddr_t)&bfreelist, PRIBIO+1);
		goto loop;
	}
	spl0();
	notavail(bp = bfreelist.av_forw);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	bp->b_flags = B_BUSY;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = dp->b_forw;
	bp->b_back = dp;
	dp->b_forw->b_back = bp;
	dp->b_forw = bp;
	bp->b_dev = dev;
	bp->b_blkno = blkno;
	return(bp);
}

/*
 * get an empty block,
 * not assigned to any particular device
 */
struct buf *
geteblk()
{
	register struct buf *bp;
	register struct buf *dp;

loop:
	spl6();
	while (bfreelist.av_forw == &bfreelist) {
		bfreelist.b_flags |= B_WANTED;
		sleep((caddr_t)&bfreelist, PRIBIO+1);
	}
	spl0();
	dp = &bfreelist;
	notavail(bp = bfreelist.av_forw);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	bp->b_flags = B_BUSY;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = dp->b_forw;
	bp->b_back = dp;
	dp->b_forw->b_back = bp;
	dp->b_forw = bp;
	bp->b_dev = (dev_t)NODEV;
	return(bp);
}

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
iowait(bp)
register struct buf *bp;
{

	spl6();
	while ((bp->b_flags&B_DONE)==0)
		sleep((caddr_t)bp, PRIBIO);
	spl0();
	geterror(bp);
}

/*
 * Unlink a buffer from the available list and mark it busy.
 * (internal interface)
 */
notavail(bp)
register struct buf *bp;
{
	register s;

	s = spl6();
	bp->av_back->av_forw = bp->av_forw;
	bp->av_forw->av_back = bp->av_back;
	bp->b_flags |= B_BUSY;
	splx(s);
}

/*
 * Mark I/O complete on a buffer, release it if I/O is asynchronous,
 * and wake up anyone waiting for it.
 */
iodone(bp)
register struct buf *bp;
{

	bp->b_flags |= B_DONE;
	if (bp->b_flags&B_ASYNC)
		brelse(bp);
	else {
		bp->b_flags &= ~B_WANTED;
		wakeup((caddr_t)bp);
	}
}

/*
 * Zero the core associated with a buffer.
 */
clrbuf(bp)
struct buf *bp;
{
	register *p;
	register c;

	p = bp->b_un.b_words;
	c = BSIZE/sizeof(int);
	do
		*p++ = 0;
	while (--c);
	bp->b_resid = 0;
}

/*
 * swap I/O
 */
swap(p,blkno, coreaddr, count, rdflg,uflag)
	struct  proc  *p;
{
	register struct buf *bp;
	register int c, pt, i;
	int ptab[(MAXUMEM+127)/128];
	extern	int	Swap2map[], swap2utl[];

	bp = &swbuf1;
	if(bp->b_flags & B_BUSY)
		if((swbuf2.b_flags&B_WANTED) == 0)
			bp = &swbuf2;
	spl6();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PSWP+1);
	}
	if (uflag) {	/* get current page table size */
		ptaccess(p, Swap2map, swap2utl);
		pt = ((struct user *)swap2utl)->u_pcb.pcb_szpt;
		for(i=pt; --i>=0; )
			ptab[i] = (Swap2map+UPAGES)[i]&PG_PFNUM;
	}
	bp->b_proc = p;
	bp->b_un.b_addr = (caddr_t)ctob(coreaddr);
	bp->b_flags = B_BUSY | B_PHYS | rdflg | (uflag ? B_UAREA : 0);
	while (count > 0) {
		bp->b_dev = swapdev;
		bp->b_bcount = ctob( (c = count>120 ? 120 : count));
		bp->b_blkno = swplo+blkno;
		(*bdevsw[major(swapdev)].d_strategy)(bp);
		spl6();
		while((bp->b_flags&B_DONE)==0)
			sleep((caddr_t)bp, PSWP);
		if (bp->b_flags&B_WANTED)
			wakeup((caddr_t)bp);
		spl0();
		bp->b_un.b_addr += ctob(c);
		if (bp->b_flags & B_UAREA) {
			bp->b_un.b_addr -= ctob(UPAGES);
			if (rdflg == B_READ) {	/* don't let page table size change */
				spl6();
				ptaccess(p, Swap2map, swap2utl);
				((struct user *)swap2utl)->u_pcb.pcb_szpt = pt;
				for(i=pt; --i>=0; )
				   ((struct user *)swap2utl)->u_ptable[i]
					= ptab[i];
				spl0();
			}
		}
		bp->b_flags &= ~(B_WANTED | B_UAREA | B_DONE);
		if (bp->b_flags & B_ERROR)
			panic("IO err in swap");
		count -= c;
		blkno += c;
	}
	bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS);
}

/*
 * make sure all write-behind blocks
 * on dev (or NODEV for all)
 * are flushed out.
 * (from umount and update)
 */
bflush(dev)
dev_t dev;
{
	register struct buf *bp;

loop:
	spl6();
	for (bp = bfreelist.av_forw; bp != &bfreelist; bp = bp->av_forw) {
		if (bp->b_flags&B_DELWRI && (dev == NODEV||dev==bp->b_dev)) {
			bp->b_flags |= B_ASYNC;
			notavail(bp);
			bwrite(bp);
			goto loop;
		}
	}
	spl0();
}

/*
 * Raw I/O. The arguments are
 *	The strategy routine for the device
 *	A buffer, which will always be a special buffer
 *	  header owned exclusively by the device for this purpose
 *	The device number
 *	Read/write flag
 * Essentially all the work is computing physical addresses and
 * validating them.
 */
physio(strat, bp, dev, rw)
register struct buf *bp;
int (*strat)();
{
	register int c;
	int flag = 0;

	if (useracc(u.u_base,u.u_count,rw) == NULL) {
		u.u_error =EFAULT;
		return; }
	spl6();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO+1);
	}
	bp->b_error = 0;
	bp->b_proc = u.u_procp;
	bp->b_un.b_addr = u.u_base;
	while (u.u_count > 0 && bp->b_error==0) {
		bp->b_flags = B_BUSY | B_PHYS | rw;
		bp->b_dev = dev;
		bp->b_blkno = u.u_offset >> BSHIFT;
		bp->b_bcount = c = u.u_count>60*1024 ? 60*1024 : u.u_count;
		flag = u.u_procp->p_flag & SULOCK;
		u.u_procp->p_flag |= SULOCK;
		(*strat)(bp);
		spl6();
		while ((bp->b_flags&B_DONE) == 0)
			sleep((caddr_t)bp, PRIBIO);
		u.u_procp->p_flag &= ~SULOCK;
		u.u_procp->p_flag |= flag;
		if (bp->b_flags&B_WANTED)
			wakeup((caddr_t)bp);
		spl0();
		bp->b_un.b_addr += c;
		u.u_count -= c;
		u.u_offset += c;
	}
	bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS);
	u.u_count = bp->b_resid;
	geterror(bp);
	return;
}

/*
 * Pick up the device's error number and pass it to the user;
 * if there is an error but the number is 0 set a generalized
 * code.  Actually the latter is always true because devices
 * don't yet return specific errors.
 */
geterror(bp)
register struct buf *bp;
{

	if (bp->b_flags&B_ERROR)
		if ((u.u_error = bp->b_error)==0)
			u.u_error = EIO;
}
