/*	init_main.c	3.3	%H%	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/filsys.h"
#include "../h/mount.h"
#include "../h/map.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/seg.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
#include "../h/clock.h"
#include "../h/vm.h"
#include "../h/cmap.h"

/*
 * Initialization code.
 * Called from cold start routine as
 * soon as a stack and segmentation
 * have been established.
 * Functions:
 *	clear and free user core
 *	turn on clock
 *	hand craft 0th process
 *	call all initialization routines
 *	fork - process 0 to schedule
 *	     - process 2 to page out
 *	     - process 1 execute bootstrap
 *
 * loop at loc 13 (0xd) in user mode -- /etc/init
 *	cannot be executed.
 */
main(firstaddr)
{

	cpusid = mfpr(SID);		/* get system identification */
#ifdef FASTVAX
	rqinit();
#endif
	startup(firstaddr);
	if (lotsfree == 0)
		lotsfree = LOTSFREE;

	/*
	 * set up system process 0 (swapper)
	 */

	proc[0].p_p0br = (struct pte *)mfpr(P0BR);
	proc[0].p_szpt = 1;
	proc[0].p_addr = uaddr(&proc[0]);
	proc[0].p_stat = SRUN;
	proc[0].p_flag |= SLOAD|SSYS;
	proc[0].p_nice = NZERO;
	u.u_procp = &proc[0];
	u.u_cmask = CMASK;
	clkstart();

	/*
	 * Initialize devices and
	 * set up 'known' i-nodes
	 */

	ihinit();
	bhinit();
	cinit();
	binit();
	bswinit();
	iinit();
	rootdir = iget(rootdev, (ino_t)ROOTINO);
	rootdir->i_flag &= ~ILOCK;
	u.u_cdir = iget(rootdev, (ino_t)ROOTINO);
	u.u_cdir->i_flag &= ~ILOCK;
	u.u_rdir = NULL;
	u.u_dmap = zdmap;
	u.u_smap = zdmap;

	/*
	 * make page-out daemon (process 2)
	 * the daemon has ctopt(NSWBUF*CLSIZE*KLMAX) pages of page
	 * table so that it can map dirty pages into
	 * its address space during asychronous pushes.
	 */

	mpid = 1;
	proc[0].p_szpt = clrnd(ctopt(NSWBUF*CLSIZE*KLMAX + UPAGES));
	proc[1].p_stat = SZOMB;		/* force it to be in proc slot 2 */
	if (newproc(0)) {
		proc[2].p_flag |= SLOAD|SSYS;
		proc[2].p_dsize = u.u_dsize = NSWBUF*CLSIZE*KLMAX; 
		pageout();
	}

	/*
	 * make init process and
	 * enter scheduling loop
	 */

	mpid = 0;
	proc[1].p_stat = 0;
	proc[0].p_szpt = CLSIZE;
	if (newproc(0)) {
		expand(clrnd((int)btoc(szicode)), P0BR);
		VOID copyout((caddr_t)icode, (caddr_t)0, (unsigned)szicode);
		/*
		 * Return goes to loc. 0 of user init
		 * code just copied out.
		 */
		return;
	}
	proc[0].p_szpt = 1;
	sched();
}

/*
 * iinit is called once (from main)
 * very early in initialization.
 * It reads the root's super block
 * and initializes the current date
 * from the last modified date.
 *
 * panic: iinit -- cannot read the super
 * block. Usually because of an IO error.
 */
iinit()
{
	register struct buf *cp, *bp;
	register struct filsys *fp;
	register unsigned i, j;

	(*bdevsw[major(rootdev)].d_open)(rootdev, 1);
	bp = bread(rootdev, SUPERB);
	cp = geteblk();
	if(u.u_error)
		panic("iinit");
	bcopy(bp->b_un.b_addr, cp->b_un.b_addr, sizeof(struct filsys));
	brelse(bp);
	mount[0].m_bufp = cp;
	mount[0].m_dev = rootdev;
	fp = cp->b_un.b_filsys;
	fp->s_flock = 0;
	fp->s_ilock = 0;
	fp->s_ronly = 0;
	fp->s_lasti = 1;
	fp->s_nbehind = 0;
	/* on boot, read VAX TODR register (GMT 10 ms.
	*	clicks into current year) and set software time
	*	in 'int time' (GMT seconds since year YRREF)
	*/
	for (i = 0 , j = YRREF ; j < YRCURR ; j++)
		i += (SECYR + (j%4?0:SECDAY)) ;
	time = udiv(mfpr(TODR),100) + i ;
	bootime = time;
}

/*
 * This is the set of buffers proper, whose heads
 * were declared in buf.h.  There can exist buffer
 * headers not pointing here that are used purely
 * as arguments to the I/O routines to describe
 * I/O to be done-- e.g. swap headers swbuf[] for
 * swapping.
 */
char	buffers[NBUF][BSIZE];

/*
 * Initialize the buffer I/O system by freeing
 * all buffers and setting all device buffer lists to empty.
 *
 * SHOULD USE MEMALL HERE!!!
 */
binit()
{
	register struct buf *bp;
	register struct buf *dp;
	register int i;
	struct bdevsw *bdp;

	bfreelist.b_forw = bfreelist.b_back =
	    bfreelist.av_forw = bfreelist.av_back = &bfreelist;
	for (i=0; i<NBUF; i++) {
		bp = &buf[i];
		bp->b_dev = NODEV;
		bp->b_un.b_addr = buffers[i];
		bp->b_back = &bfreelist;
		bp->b_forw = bfreelist.b_forw;
		bfreelist.b_forw->b_back = bp;
		bfreelist.b_forw = bp;
		bp->b_flags = B_BUSY;
		brelse(bp);
	}
	for (bdp = bdevsw; bdp->d_open; bdp++) {
		dp = bdp->d_tab;
		if(dp) {
			dp->b_forw = dp;
			dp->b_back = dp;
		}
		nblkdev++;
	}
}

/*
 * Initialize linked list of free swap
 * headers. These do not actually point
 * to buffers, but rather to pages that
 * are being swapped in and out.
 */
bswinit()
{
	register int i;

	bswlist.av_forw = &swbuf[0];
	for (i=0; i<NSWBUF-1; i++)
		swbuf[i].av_forw = &swbuf[i+1];
	swbuf[NSWBUF-1].av_forw = NULL;
}
