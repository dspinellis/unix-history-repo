/*	init_main.c	4.29	82/04/19	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/fs.h"
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
#include "../h/text.h"
#include "../h/vlimit.h"
#include "../h/clist.h"
#ifdef INET
#include "../h/protosw.h"
#endif

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
	register int i;
	register struct proc *p;
	struct fs *fsp;

	rqinit();
#include "loop.h"
	startup(firstaddr);

	/*
	 * set up system process 0 (swapper)
	 */
	p = &proc[0];
	p->p_p0br = (struct pte *)mfpr(P0BR);
	p->p_szpt = 1;
	p->p_addr = uaddr(p);
	p->p_stat = SRUN;
	p->p_flag |= SLOAD|SSYS;
	p->p_nice = NZERO;
	setredzone(p->p_addr, (caddr_t)&u);
	u.u_procp = p;
	u.u_cmask = CMASK;
	for (i = 1; i < sizeof(u.u_limit)/sizeof(u.u_limit[0]); i++)
		switch (i) {

		case LIM_STACK:
			u.u_limit[i] = 512*1024;
			continue;
		case LIM_DATA:
			u.u_limit[i] = ctob(MAXDSIZ);
			continue;
		default:
			u.u_limit[i] = INFINITY;
			continue;
		}
	p->p_maxrss = INFINITY/NBPG;
	clkstart();

	/*
	 * Initialize tables, protocols, and set up well-known inodes.
	 */
	mbinit();
	cinit();			/* needed by dmc-11 driver */
#ifdef INET
#if NLOOP > 0
	loattach();			/* XXX */
#endif
	ifinit();
	pfinit();			/* must follow interfaces */
#endif
	ihinit();
	bhinit();
	binit();
	bswinit();
	iinit();
	fsp = getfs(rootdev);
	rootdir = iget(rootdev, fsp, (ino_t)ROOTINO);
	rootdir->i_flag &= ~ILOCK;
	u.u_cdir = iget(rootdev, fsp, (ino_t)ROOTINO);
	u.u_cdir->i_flag &= ~ILOCK;
	u.u_rdir = NULL;
	u.u_dmap = zdmap;
	u.u_smap = zdmap;

	/*
	 * Set the scan rate and other parameters of the paging subsystem.
	 */
	setupclock();

	/*
	 * make page-out daemon (process 2)
	 * the daemon has ctopt(nswbuf*CLSIZE*KLMAX) pages of page
	 * table so that it can map dirty pages into
	 * its address space during asychronous pushes.
	 */
	mpid = 1;
	proc[0].p_szpt = clrnd(ctopt(nswbuf*CLSIZE*KLMAX + UPAGES));
	proc[1].p_stat = SZOMB;		/* force it to be in proc slot 2 */
	if (newproc(0)) {
		proc[2].p_flag |= SLOAD|SSYS;
		proc[2].p_dsize = u.u_dsize = nswbuf*CLSIZE*KLMAX; 
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
		(void) swpexpand(u.u_dsize, 0, &u.u_dmap, &u.u_smap);
		(void) copyout((caddr_t)icode, (caddr_t)0, (unsigned)szicode);
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
	register struct buf *bp;
	register struct fs *fp;
	register int i;
	int blks;

	(*bdevsw[major(rootdev)].d_open)(rootdev, 1);
	bp = bread(rootdev, SBLOCK, SBSIZE);
	if(u.u_error)
		panic("iinit");
	bp->b_flags |= B_LOCKED;		/* block can never be re-used */
	brelse(bp);
	mount[0].m_dev = rootdev;
	mount[0].m_bufp = bp;
	fp = bp->b_un.b_fs;
	if (fp->fs_magic != FS_MAGIC)
		panic("root bad magic number");
	if (fp->fs_bsize > MAXBSIZE)
		panic("root fs_bsize too big");
	fp->fs_ronly = 0;
	fp->fs_fsmnt[0] = '/';
	for (i = 1; i < sizeof(fp->fs_fsmnt); i++)
		fp->fs_fsmnt[i] = 0;
	blks = howmany(fp->fs_cssize, fp->fs_fsize);
	for (i = 0; i < blks; i += fp->fs_frag) {
		bp = bread(rootdev, fsbtodb(fp, fp->fs_csaddr + i),
		    blks - i < fp->fs_frag ?
			(blks - i) * fp->fs_fsize :
			fp->fs_bsize);
		if (u.u_error)
			panic("root can't read csum");
		fp->fs_csp[i / fp->fs_frag] = bp->b_un.b_cs;
		bp->b_flags |= B_LOCKED;
		brelse(bp);
	}
}

/*
 * Initialize the buffer I/O system by freeing
 * all buffers and setting all device buffer lists to empty.
 */
binit()
{
	register struct buf *bp;
	register struct buf *dp;
	register int i;
	struct bdevsw *bdp;
	struct swdevt *swp;

	for (dp = bfreelist; dp < &bfreelist[BQUEUES]; dp++) {
		dp->b_forw = dp->b_back = dp->av_forw = dp->av_back = dp;
		dp->b_flags = B_HEAD;
	}
	dp--;				/* dp = &bfreelist[BQUEUES-1]; */
	for (i = 0; i < nbuf; i++) {
		bp = &buf[i];
		bp->b_dev = NODEV;
		bp->b_un.b_addr = buffers + i * MAXBSIZE;
		bp->b_bcount = MAXBSIZE;
		bp->b_back = dp;
		bp->b_forw = dp->b_forw;
		dp->b_forw->b_back = bp;
		dp->b_forw = bp;
		bp->b_flags = B_BUSY|B_INVAL;
		brelse(bp);
	}
	for (bdp = bdevsw; bdp->d_open; bdp++)
		nblkdev++;
	/*
	 * Count swap devices, and adjust total swap space available.
	 * Some of this space will not be available until a vswapon()
	 * system is issued, usually when the system goes multi-user.
	 */
	nswdev = 0;
	for (swp = swdevt; swp->sw_dev; swp++)
		nswdev++;
	if (nswdev == 0)
		panic("binit");
	if (nswdev > 1)
		nswap = (nswap/DMMAX)*DMMAX;
	nswap *= nswdev;
	maxpgio *= nswdev;
	swfree(0);
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
	register struct buf *sp = swbuf;

	bswlist.av_forw = sp;
	for (i=0; i<nswbuf-1; i++, sp++)
		sp->av_forw = sp+1;
	sp->av_forw = NULL;
}

/*
 * Initialize clist by freeing all character blocks, then count
 * number of character devices. (Once-only routine)
 */
cinit()
{
	register int ccp;
	register struct cblock *cp;
	register struct cdevsw *cdp;

	ccp = (int)cfree;
	ccp = (ccp+CROUND) & ~CROUND;
	for(cp=(struct cblock *)ccp; cp < &cfree[nclist-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
		cfreecount += CBSIZE;
	}
	ccp = 0;
	for(cdp = cdevsw; cdp->d_open; cdp++)
		ccp++;
	nchrdev = ccp;
}
