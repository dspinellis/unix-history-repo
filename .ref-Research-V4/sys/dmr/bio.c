#include "/sys/nsys/param.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/buf.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/systm.h"

char	buffers[NBUF][512];
struct	buf	swbuf;

struct	{ int int;};
#define	PS	0177776
#define	JTM	3

bread(dev, blkno)
{
	struct buf *bp;
	register struct buf *rbp;

	rbp = bp = getblk(dev, blkno);
	if (rbp->b_flags&B_DONE)
		return(rbp);
	rbp->b_flags =| B_READ;
	rbp->b_wcount = -256;
	(*bdevsw[dev.d_major].d_strategy)(rbp);
	iowait(bp);
	return(bp);
}

bwrite(bp)
struct buf *bp;
{
	register struct buf *rbp;

	rbp = bp;
	rbp->b_flags =& ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	rbp->b_wcount = -256;
	(*bdevsw[rbp->b_dev.d_major].d_strategy)(rbp);
	if ((bp->b_flags&B_ASYNC) == 0) {
		iowait(bp);
		brelse(bp);
	}
}

bdwrite(bp)
struct buf *bp;
{
	register struct buf *rbp;

	rbp = bp;
	if (rbp->b_dev.d_major == JTM)
		bawrite(rbp);
	else {
		rbp->b_flags =| B_DELWRI | B_DONE;
		brelse(rbp);
	}
}

bawrite(bp)
struct buf *bp;
{
	bp->b_flags =| B_ASYNC;
	bwrite(bp);
}

brelse(bp)
struct buf *bp;
{
	register struct buf *rbp;
	register int sps;

	if (bp->b_flags&B_WANTED)
		wakeup(bp);
	if (bfreelist.b_flags&B_WANTED) {
		bfreelist.b_flags =& ~B_WANTED;
		wakeup(&bfreelist);
	}
	rbp = bp;
	sps = PS->int;
	PS->int =| 0300;
	rbp->b_flags =& ~(B_WANTED|B_BUSY|B_ASYNC);
	bfreelist.av_back->av_forw = rbp;
	rbp->av_back = bfreelist.av_back;
	bfreelist.av_back = rbp;
	rbp->av_forw = &bfreelist;
	PS->int = sps;
}

getblk(dev, blkno)
{
	register struct buf *bp;
	struct buf *bp1;
	struct devtab *dp;
	register struct devtab *rdp;
	extern lbolt;

    loop:
	if (dev < 0)
		dp = &bfreelist;
	else {
		dp = &devtab[dev.d_major];
		for (bp=dp->b_forw; bp != dp; bp = bp->b_forw) {
			if (bp->b_blkno!=blkno || bp->b_dev!=dev)
				continue;
			if (bp->b_flags&B_BUSY) {
				bp->b_flags =| B_WANTED;
				sleep(bp, PRIBIO);
				goto loop;
			}
			bp1 = bp;
			notavail(bp);
			return(bp1);
		}
	}
	spl6();
	if (bfreelist.av_forw == bfreelist.av_back) {
		bfreelist.b_flags =| B_WANTED;
		sleep(&bfreelist, PRIBIO);
		spl0();
		goto loop;
	}
	spl0();
	notavail(bp1 = bfreelist.av_forw);
	bp = bp1;
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags =| B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	rdp = dp;
	bp->b_flags = B_BUSY | B_RELOC;
	bp->b_back->b_forw = bp->b_forw;
	bp->b_forw->b_back = bp->b_back;
	bp->b_forw = rdp->b_forw;
	bp->b_back = rdp;
	rdp->b_forw->b_back = bp;
	rdp->b_forw = bp;
	bp->b_dev = dev;
	bp->b_blkno = blkno;
	return(bp);
}

iowait(bp)
struct buf *bp;
{
	spl6();
	while ((bp->b_flags&B_DONE)==0)
		sleep(bp, PRIBIO);
	spl0();
	if (bp->b_flags&B_ERROR)
		u.u_error = EIO;
}

notavail(bp)
struct buf *bp;
{
	register struct buf *rbp;
	register int sps;

	rbp = bp;
	sps = PS->int;
	PS->int =| 0300;
	rbp->av_back->av_forw = rbp->av_forw;
	rbp->av_forw->av_back = rbp->av_back;
	rbp->b_flags =| B_BUSY;
	PS->int = sps;
}

iodone(bp)
struct buf *bp;
{
	register struct buf *rbp;

	rbp = bp;
	rbp->b_flags =| B_DONE;
	if (rbp->b_flags&B_ASYNC)
		brelse(rbp);
	else {
		rbp->b_flags =& ~B_WANTED;
		wakeup(rbp);
	}
}

clrbuf(bp)
int *bp;
{
	register *p, *ep;

	p = bp->b_addr;
	for (ep = p+256; p<ep;)
		*p++ = 0;
}

binit()
{
	register struct buf *bp;
	register struct devtab *dp;
	int i;

	bfreelist.b_forw = bfreelist.b_back =
	    bfreelist.av_forw = bfreelist.av_back = &bfreelist;
	for (i=0; i<NBUF; i++) {
		bp = &buf[i];
		bp->b_dev = -1;
		bp->b_addr = buffers[i];
		bp->b_back = &bfreelist;
		bp->b_forw = bfreelist.b_forw;
		bfreelist.b_forw->b_back = bp;
		bfreelist.b_forw = bp;
		brelse(bp);
	}
	for (dp = devtab; dp < &devtab[NMAJ]; dp++) {
		dp->b_forw = dp;
		dp->b_back = dp;
	}
}

/*
 * Device start routine for disks
 */
#define	IENABLE	0100
#define	WCOM	02
#define	RCOM	04
#define	GO	01
devstart(bp, devloc, devblk)
struct buf *bp;
int *devloc;
{
	register int *dp;
	register struct buf *rbp;
	register int com;

	dp = devloc;
	rbp = bp;
	*dp = devblk;			/* block address */
	*--dp = rbp->b_addr;		/* buffer address */
	*--dp = rbp->b_wcount;		/* word count */
	com = IENABLE|GO | rbp->b_flags&B_XMEM;
	if (rbp->b_flags&B_READ)	/* command + x-mem */
		com =| RCOM;
	else
		com =| WCOM;
	*--dp = com;
}

/*
 * swap I/O
 */
swap(blkno, coreaddr, count, rdflg)
{
	spl6();
	while (swbuf.b_flags&B_BUSY) {
		swbuf.b_flags =| B_WANTED;
		sleep(&swbuf.b_flags, PSWP);
	}
	swbuf.b_flags = B_BUSY | rdflg;
	swbuf.b_dev = SWAPDEV;
	swbuf.b_wcount = - (count<<5);	/* 32 w/block */
	swbuf.b_blkno = blkno;
	swbuf.b_addr = coreaddr<<6;	/* 64 b/block */
	swbuf.b_flags =| (coreaddr>>6)&B_XMEM;
	(*bdevsw[SWAPDEV>>8].d_strategy)(&swbuf);
	spl6();
	while((swbuf.b_flags&B_DONE)==0)
		sleep(&swbuf, PSWP);
	if (swbuf.b_flags&B_WANTED)
		wakeup(&swbuf.b_flags);
	spl0();
	swbuf.b_flags =& ~(B_BUSY|B_WANTED);
	return(swbuf.b_flags&B_ERROR);
}

/*
 * make sure all write-behind blocks
 * on dev (or NODEV for all)
 * are flushed out.
 */
bflush(dev)
{
	struct buf *nbp, *xbp;
	register struct buf *bp;

	spl6();
	for (nbp = bfreelist.av_forw; nbp != &bfreelist; ) {
		bp = nbp;
		nbp = bp->av_forw;
		if (bp->b_flags&B_DELWRI && (dev == -1||dev==bp->b_dev)) {
			bp->b_flags =| B_ASYNC;
			xbp = bp;
			notavail(bp);
			bwrite(xbp);
		}
	}
	spl0();
}
