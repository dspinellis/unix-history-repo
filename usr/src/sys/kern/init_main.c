/*	init_main.c	6.9	85/05/27	*/

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "fs.h"
#include "mount.h"
#include "map.h"
#include "proc.h"
#include "inode.h"
#include "seg.h"
#include "conf.h"
#include "buf.h"
#include "vm.h"
#include "cmap.h"
#include "text.h"
#include "clist.h"
#ifdef INET
#include "protosw.h"
#endif
#include "quota.h"
#include "../machine/reg.h"
#include "../machine/cpu.h"

int	cmask = CMASK;
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
 *	     - process 1 execute bootstrap
 *	     - process 2 to page out
 */
main(firstaddr)
	int firstaddr;
{
	register int i;
	register struct proc *p;
	struct fs *fs;
	int s;

	rqinit();
#include "loop.h"
	startup(firstaddr);

	/*
	 * set up system process 0 (swapper)
	 */
	p = &proc[0];
	p->p_p0br = u.u_pcb.pcb_p0br;
	p->p_szpt = 1;
	p->p_addr = uaddr(p);
	p->p_stat = SRUN;
	p->p_flag |= SLOAD|SSYS;
	p->p_nice = NZERO;
	setredzone(p->p_addr, (caddr_t)&u);
	u.u_procp = p;
#ifdef vax
	/*
	 * These assume that the u. area is always mapped 
	 * to the same virtual address. Otherwise must be
	 * handled when copying the u. area in newproc().
	 */
	u.u_nd.ni_iov = &u.u_nd.ni_iovec;
	u.u_ap = u.u_arg;
#endif
	u.u_nd.ni_iovcnt = 1;
	u.u_cmask = cmask;
	u.u_lastfile = -1;
	for (i = 1; i < NGROUPS; i++)
		u.u_groups[i] = NOGROUP;
	for (i = 0; i < sizeof(u.u_rlimit)/sizeof(u.u_rlimit[0]); i++)
		u.u_rlimit[i].rlim_cur = u.u_rlimit[i].rlim_max = 
		    RLIM_INFINITY;
	/*
	 * Virtual memory limits get set in vminit().
	 */
	vminit();
#if defined(QUOTA)
	qtinit();
	p->p_quota = u.u_quota = getquota(0, 0, Q_NDQ);
#endif
	startrtclock();
#include "kg.h"
#if NKG > 0
	startkgclock();
#endif

	/*
	 * Initialize tables, protocols, and set up well-known inodes.
	 */
	mbinit();
	cinit();
#ifdef INET
#if NLOOP > 0
	loattach();			/* XXX */
#endif
	/*
	 * Block reception of incoming packets
	 * until protocols have been initialized.
	 */
	s = splimp();
	ifinit();
#endif
	domaininit();
#ifdef INET
	splx(s);
#endif
	pqinit();
	ihinit();
	bhinit();
	binit();
	bswinit();
	nchinit();
#ifdef GPROF
	kmstartup();
#endif

	fs = mountfs(rootdev, 0, (struct inode *)0);
	if (fs == 0)
		panic("iinit");
	bcopy("/", fs->fs_fsmnt, 2);

	inittodr(fs->fs_time);
	boottime = time;

/* kick off timeout driven events by calling first time */
	roundrobin();
	schedcpu();
	schedpaging();

/* set up the root file system */
	rootdir = iget(rootdev, fs, (ino_t)ROOTINO);
	iunlock(rootdir);
	u.u_cdir = iget(rootdev, fs, (ino_t)ROOTINO);
	iunlock(u.u_cdir);
	u.u_rdir = NULL;

	u.u_dmap = zdmap;
	u.u_smap = zdmap;

	/*
	 * make init process
	 */

	proc[0].p_szpt = CLSIZE;
	if (newproc(0)) {
		expand(clrnd((int)btoc(szicode)), 0);
		(void) swpexpand(u.u_dsize, 0, &u.u_dmap, &u.u_smap);
		(void) copyout((caddr_t)icode, (caddr_t)0, (unsigned)szicode);
		/*
		 * Return goes to loc. 0 of user init
		 * code just copied out.
		 */
		return;
	}
	/*
	 * make page-out daemon (process 2)
	 * the daemon has ctopt(nswbuf*CLSIZE*KLMAX) pages of page
	 * table so that it can map dirty pages into
	 * its address space during asychronous pushes.
	 */
	proc[0].p_szpt = clrnd(ctopt(nswbuf*CLSIZE*KLMAX + UPAGES));
	if (newproc(0)) {
		proc[2].p_flag |= SLOAD|SSYS;
		proc[2].p_dsize = u.u_dsize = nswbuf*CLSIZE*KLMAX; 
		pageout();
		/*NOTREACHED*/
	}

	/*
	 * enter scheduling loop
	 */
	proc[0].p_szpt = 1;
	sched();
}

/*
 * Initialize hash links for buffers.
 */
bhinit()
{
	register int i;
	register struct bufhd *bp;

	for (bp = bufhash, i = 0; i < BUFHSZ; i++, bp++)
		bp->b_forw = bp->b_back = (struct buf *)bp;
}

/*
 * Initialize the buffer I/O system by freeing
 * all buffers and setting all device buffer lists to empty.
 */
binit()
{
	register struct buf *bp, *dp;
	register int i;
	struct swdevt *swp;
	int base, residual;

	for (dp = bfreelist; dp < &bfreelist[BQUEUES]; dp++) {
		dp->b_forw = dp->b_back = dp->av_forw = dp->av_back = dp;
		dp->b_flags = B_HEAD;
	}
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < nbuf; i++) {
		bp = &buf[i];
		bp->b_dev = NODEV;
		bp->b_bcount = 0;
		bp->b_un.b_addr = buffers + i * MAXBSIZE;
		if (i < residual)
			bp->b_bufsize = (base + 1) * CLBYTES;
		else
			bp->b_bufsize = base * CLBYTES;
		binshash(bp, &bfreelist[BQ_AGE]);
		bp->b_flags = B_BUSY|B_INVAL;
		brelse(bp);
	}
	/*
	 * Count swap devices, and adjust total swap space available.
	 * Some of this space will not be available until a vswapon()
	 * system is issued, usually when the system goes multi-user.
	 */
	nswdev = 0;
	nswap = 0;
	for (swp = swdevt; swp->sw_dev; swp++) {
		nswdev++;
		if (swp->sw_nblks > nswap)
			nswap = swp->sw_nblks;
	}
	if (nswdev == 0)
		panic("binit");
	if (nswdev > 1)
		nswap = ((nswap + dmmax - 1) / dmmax) * dmmax;
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

	ccp = (int)cfree;
	ccp = (ccp+CROUND) & ~CROUND;
	for(cp=(struct cblock *)ccp; cp < &cfree[nclist-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
		cfreecount += CBSIZE;
	}
}
