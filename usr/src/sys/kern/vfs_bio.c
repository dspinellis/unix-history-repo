/*	vfs_bio.c	3.3	%H%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/pte.h"
#include "../h/vm.h"

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
 *	baddr	(if it is incore)
 * Eventually the buffer must be released, possibly with the
 * side effect of writing it out, by using one of
 *	bwrite
 *	bdwrite
 *	bawrite
 *	brelse
 */

#define	BUFHSZ	63
#define	BUFHASH(blkno)	(blkno % BUFHSZ)
short	bufhash[BUFHSZ];

/*
 * Initialize hash links for buffers.
 */
bhinit()
{
	register int i;

	for (i = 0; i < BUFHSZ; i++)
		bufhash[i] = -1;
}

/* #define	DISKMON	1 */

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
 * Swap IO headers -
 * They contain the necessary information for the swap I/O.
 * At any given time, a swap header can be in three
 * different lists. When free it is in the free list, 
 * when allocated and the I/O queued, it is on the swap 
 * device list, and finally, if the operation was a dirty 
 * page push, when the I/O completes, it is inserted 
 * in a list of cleaned pages to be processed by the pageout daemon.
 */
struct	buf swbuf[NSWBUF];
short	swsize[NSWBUF];		/* CAN WE JUST USE B_BCOUNT? */
int	swpf[NSWBUF];


#ifdef	FASTVAX
#define	notavail(bp) \
{ \
	int s = spl6(); \
	(bp)->av_back->av_forw = (bp)->av_forw; \
	(bp)->av_forw->av_back = (bp)->av_back; \
	(bp)->b_flags |= B_BUSY; \
	splx(s); \
}
#endif

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
	u.u_vm.vm_inblk++;		/* pay for read */
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
			u.u_vm.vm_inblk++;		/* pay for read */
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
			u.u_vm.vm_inblk++;		/* pay in advance */
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
	if ((flag&B_DELWRI) == 0)
		u.u_vm.vm_oublk++;		/* noone paid yet */
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

	if ((bp->b_flags&B_DELWRI) == 0)
		u.u_vm.vm_oublk++;		/* noone paid yet */
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
	if ((bp->b_flags&B_ERROR) && bp->b_dev != NODEV) {
		bunhash(bp);
		bp->b_dev = NODEV;  /* no assoc. on error */
	}
	s = spl6();
	if(bp->b_flags & (B_AGE|B_ERROR)) {
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
	register int dblkno = fsbtodb(blkno);

	for (bp = &buf[bufhash[BUFHASH(blkno)]]; bp != &buf[-1];
	    bp = &buf[bp->b_hlink])
		if (bp->b_blkno == dblkno && bp->b_dev == dev)
			return (1);
	return (0);
}

struct buf *
baddr(dev, blkno)
dev_t dev;
daddr_t blkno;
{

	if (incore(dev, blkno))
		return (bread(dev, blkno));
	return (0);
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
	register struct buf *bp, *dp, *ep;
	register int i, x;
	register int dblkno = fsbtodb(blkno);

    loop:
	VOID spl0();
	for (bp = &buf[bufhash[BUFHASH(blkno)]]; bp != &buf[-1];
	    bp = &buf[bp->b_hlink]) {
		if (bp->b_blkno != dblkno || bp->b_dev != dev)
			continue;
		VOID spl6();
		if (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
			goto loop;
		}
		VOID spl0();
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
		bp->b_flags |= B_CACHE;
		return(bp);
	}
	if (major(dev) >= nblkdev)
		panic("blkdev");
	dp = bdevsw[major(dev)].d_tab;
	if (dp == NULL)
		panic("devtab");
	VOID spl6();
	if (bfreelist.av_forw == &bfreelist) {
		bfreelist.b_flags |= B_WANTED;
		sleep((caddr_t)&bfreelist, PRIBIO+1);
		goto loop;
	}
	spl0();
	bp = bfreelist.av_forw;
	notavail(bp);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	if (bp->b_dev == NODEV)
		goto done;
	/* INLINE EXPANSION OF bunhash(bp) */
	i = BUFHASH(dbtofsb(bp->b_blkno));
	x = bp - buf;
	if (bufhash[i] == x) {
		bufhash[i] = bp->b_hlink;
	} else {
		for (ep = &buf[bufhash[i]]; ep != &buf[-1];
		    ep = &buf[ep->b_hlink])
			if (ep->b_hlink == x) {
				ep->b_hlink = bp->b_hlink;
				goto done;
			}
		panic("getblk");
	}
done:
	/* END INLINE EXPANSION */
	bp->b_flags = B_BUSY;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = dp->b_forw;
	bp->b_back = dp;
	dp->b_forw->b_back = bp;
	dp->b_forw = bp;
	bp->b_dev = dev;
	bp->b_blkno = dblkno;
	i = BUFHASH(blkno);
	bp->b_hlink = bufhash[i];
	bufhash[i] = bp - buf;
	return(bp);
}

/*
 * get an empty block,
 * not assigned to any particular device
 */
struct buf *
geteblk()
{
	register struct buf *bp, *dp, *ep;
	register int i, x;

loop:
	VOID spl6();
	while (bfreelist.av_forw == &bfreelist) {
		bfreelist.b_flags |= B_WANTED;
		sleep((caddr_t)&bfreelist, PRIBIO+1);
	}
	VOID spl0();
	dp = &bfreelist;
	bp = bfreelist.av_forw;
	notavail(bp);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	if (bp->b_dev != NODEV)
		bunhash(bp);
	bp->b_flags = B_BUSY;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = dp->b_forw;
	bp->b_back = dp;
	dp->b_forw->b_back = bp;
	dp->b_forw = bp;
	bp->b_dev = (dev_t)NODEV;
	bp->b_hlink = -1;
	return(bp);
}

bunhash(bp)
	register struct buf *bp;
{
	register struct buf *ep;
	register int i, x;

	if (bp->b_dev == NODEV)
		return;
	i = BUFHASH(dbtofsb(bp->b_blkno));
	x = bp - buf;
	if (bufhash[i] == x) {
		bufhash[i] = bp->b_hlink;
		return;
	}
	for (ep = &buf[bufhash[i]]; ep != &buf[-1];
	    ep = &buf[ep->b_hlink])
		if (ep->b_hlink == x) {
			ep->b_hlink = bp->b_hlink;
			return;
		}
	panic("bunhash");
}

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
iowait(bp)
register struct buf *bp;
{

	VOID spl6();
	while ((bp->b_flags&B_DONE)==0)
		sleep((caddr_t)bp, PRIBIO);
	VOID spl0();
	geterror(bp);
}

#ifndef FASTVAX
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
#endif

/*
 * Mark I/O complete on a buffer. If the header
 * indicates a dirty page push completion, the
 * header is inserted into the ``cleaned'' list
 * to be processed by the pageout daemon. Otherwise
 * release it if I/O is asynchronous, and wake 
 * up anyone waiting for it.
 */
iodone(bp)
register struct buf *bp;
{
	register int s;

	bp->b_flags |= B_DONE;
	if (bp->b_flags & B_DIRTY) {
		if (bp->b_flags & B_ERROR)
			panic("IO err in push");
		s = spl6();
		cnt.v_pgout++;
		bp->av_forw = bclnlist;
		bp->b_bcount = swsize[bp - swbuf];
		bp->b_pfcent = swpf[bp - swbuf];
		bclnlist = bp;
		if (bswlist.b_flags & B_WANTED)
			wakeup((caddr_t)&proc[2]);
		splx(s);
	}
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
 * swap I/O -
 *
 * If the flag indicates a dirty page push initiated
 * by the pageout daemon, we map the page into the i th
 * virtual page of process 2 (the daemon itself) where i is
 * the index of the swap header that has been allocated.
 * We simply initialize the header and queue the I/O but
 * do not wait for completion. When the I/O completes,
 * iodone() will link the header to a list of cleaned
 * pages to be processed by the pageout daemon.
 */
swap(p, dblkno, addr, nbytes, rdflg, flag, dev, pfcent)
	struct proc *p;
	swblk_t dblkno;
	caddr_t addr;
	int flag, nbytes;
	dev_t dev;
	unsigned pfcent;
{
	register struct buf *bp;
	register int c;
	int p2dp;
	register struct pte *dpte, *vpte;

	VOID spl6();
	while (bswlist.av_forw == NULL) {
		bswlist.b_flags |= B_WANTED;
		sleep((caddr_t)&bswlist, PSWP+1);
	}
	bp = bswlist.av_forw;
	bswlist.av_forw = bp->av_forw;
	VOID spl0();

	bp->b_flags = B_BUSY | B_PHYS | rdflg | flag;
	if ((bp->b_flags & (B_DIRTY|B_PGIN)) == 0)
		if (rdflg == B_READ)
			sum.v_pswpin += btoc(nbytes);
		else
			sum.v_pswpout += btoc(nbytes);
	bp->b_proc = p;
	if (flag & B_DIRTY) {
		p2dp = ((bp - swbuf) * CLSIZE) * KLMAX;
		dpte = dptopte(&proc[2], p2dp);
		vpte = vtopte(p, btop(addr));
		for (c = 0; c < nbytes; c += NBPG) {
			if (vpte->pg_pfnum == 0 || vpte->pg_fod)
				panic("swap bad pte");
			*dpte++ = *vpte++;
		}
		bp->b_un.b_addr = (caddr_t)ctob(p2dp);
	} else
		bp->b_un.b_addr = addr;
	while (nbytes > 0) {
		c = imin(ctob(120), nbytes);
		bp->b_bcount = c;
		bp->b_blkno = dblkno;
		bp->b_dev = dev;
		if (dev == swapdev)
			bp->b_blkno += swplo;
		(*bdevsw[major(dev)].d_strategy)(bp);
		if (flag & B_DIRTY) {
			if (c < nbytes)
				panic("big push");
			swsize[bp - swbuf] = nbytes;
			swpf[bp - swbuf] = pfcent;
			return;
		}
		VOID spl6();
		while((bp->b_flags&B_DONE)==0)
			sleep((caddr_t)bp, PSWP);
		VOID spl0();
		bp->b_un.b_addr += c;
		bp->b_flags &= ~B_DONE;
		if (bp->b_flags & B_ERROR) {
			if ((flag & (B_UAREA|B_PAGET)) || rdflg == B_WRITE)
				panic("hard IO err in swap");
			swkill(p, (char *)0);
		}
		nbytes -= c;
		dblkno += btoc(c);
	}
	VOID spl6();
	bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS|B_PAGET|B_UAREA|B_DIRTY);
	bp->av_forw = bswlist.av_forw;
	bswlist.av_forw = bp;
	if (bswlist.b_flags & B_WANTED) {
		bswlist.b_flags &= ~B_WANTED;
		wakeup((caddr_t)&bswlist);
		wakeup((caddr_t)&proc[2]);
	}
	VOID spl0();
}

/*
 * If rout == 0 then killed on swap error, else
 * rout is the name of the routine where we ran out of
 * swap space.
 */
swkill(p, rout)
	struct proc *p;
	char *rout;
{

	printf("%d: ", p->p_pid);
	if (rout)
		printf("out of swap space in %s\n", rout);
	else
		printf("killed on swap error\n");
	/*
	 * To be sure no looping (e.g. in vmsched trying to
	 * swap out) mark process locked in core (as though
	 * done by user) after killing it so noone will try
	 * to swap it out.
	 */
	psignal(p, SIGKIL);
	p->p_flag |= SULOCK;
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
	VOID spl6();
	for (bp = bfreelist.av_forw; bp != &bfreelist; bp = bp->av_forw) {
		if (bp->b_flags&B_DELWRI && (dev == NODEV||dev==bp->b_dev)) {
			bp->b_flags |= B_ASYNC;
			notavail(bp);
			bwrite(bp);
			goto loop;
		}
	}
	VOID spl0();
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
 * If the user has the proper access privilidges, the process is
 * marked 'delayed unlock' and the pages involved in the I/O are
 * faulted and locked. After the completion of the I/O, the above pages
 * are unlocked.
 */
physio(strat, bp, dev, rw, mincnt)
int (*strat)(); 
register struct buf *bp;
unsigned (*mincnt)();
{
	register int c;
	char *a;

	if (useracc(u.u_base,u.u_count,rw==B_READ?B_WRITE:B_READ) == NULL) {
		u.u_error = EFAULT;
		return;
	}
	VOID spl6();
	while (bp->b_flags&B_BUSY) {
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO+1);
	}
	bp->b_error = 0;
	bp->b_proc = u.u_procp;
	bp->b_un.b_addr = u.u_base;
	while (u.u_count != 0 && bp->b_error==0) {
		bp->b_flags = B_BUSY | B_PHYS | rw;
		bp->b_dev = dev;
		bp->b_blkno = u.u_offset >> PGSHIFT;
		bp->b_bcount = u.u_count;
		(*mincnt)(bp);
		c = bp->b_bcount;
		u.u_procp->p_flag |= SPHYSIO;
		vslock(a = bp->b_un.b_addr, c);
		(*strat)(bp);
		VOID spl6();
		while ((bp->b_flags&B_DONE) == 0)
			sleep((caddr_t)bp, PRIBIO);
		vsunlock(a, c, rw);
		u.u_procp->p_flag &= ~SPHYSIO;
		if (bp->b_flags&B_WANTED)
			wakeup((caddr_t)bp);
		VOID spl0();
		bp->b_un.b_addr += c;
		u.u_count -= c;
		u.u_offset += c;
	}
	bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS);
	u.u_count = bp->b_resid;
	geterror(bp);
}

/*ARGSUSED*/
unsigned
minphys(bp)
struct buf *bp;
{

	if (bp->b_bcount > 60 * 1024)
		bp->b_bcount = 60 * 1024;
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
