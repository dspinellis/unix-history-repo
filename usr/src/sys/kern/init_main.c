/*	init_main.c	4.45	83/01/16	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/fs.h"
#include "../h/mount.h"
#include "../h/map.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/seg.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/vm.h"
#include "../h/cmap.h"
#include "../h/text.h"
#include "../h/clist.h"
#ifdef INET
#include "../h/protosw.h"
#endif
#include "../h/quota.h"

#ifdef sun
#include "../sun/reg.h"
#include "../sun/cpu.h"
#endif

extern	struct user u;		/* have to declare it somewhere! */
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
#ifdef vax
main(firstaddr)
	int firstaddr;
#endif
#ifdef sun
main(regs)
	struct regs regs;
#endif
{
	register int i;
	register struct proc *p;
	struct fs *fs;
	int s;

	rqinit();
#include "loop.h"
#ifdef vax
	startup(firstaddr);
#endif
#ifdef sun
	startup();
#endif

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
#ifdef sun
	u.u_ar0 = &regs.r_r0;
#endif
	u.u_cmask = CMASK;
	for (i = 1; i < NGROUPS; i++)
		u.u_groups[i] = -1;
	for (i = 0; i < sizeof(u.u_rlimit)/sizeof(u.u_rlimit[0]); i++)
		u.u_rlimit[i].rlim_cur = u.u_rlimit[i].rlim_max = 
		    RLIM_INFINITY;
	u.u_rlimit[RLIMIT_STACK].rlim_cur = 512*1024;
	u.u_rlimit[RLIMIT_STACK].rlim_max = ctob(MAXDSIZ);
	u.u_rlimit[RLIMIT_DATA].rlim_max =
	    u.u_rlimit[RLIMIT_DATA].rlim_cur = ctob(MAXDSIZ);
	p->p_maxrss = RLIM_INFINITY/NBPG;
#ifdef QUOTA
	qtinit();
	p->p_quota = u.u_quota = getquota(0, 0, Q_NDQ);
#endif
	startrtclock();

	/*
	 * Initialize tables, protocols, and set up well-known inodes.
	 */
	mbinit();
	cinit();			/* needed by dmc-11 driver */
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
	ihinit();
	bhinit();
	binit();
	bswinit();
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
#ifdef NOPAGING
		for (;;)
			sleep((caddr_t)&u, PSLEP);
		/*NOTREACHED*/
#else
		pageout();
		/*NOTREACHED*/
#endif
	}

	/*
	 * make init process and
	 * enter scheduling loop
	 */

	mpid = 0;
	proc[1].p_stat = 0;
	proc[0].p_szpt = CLSIZE;
	if (newproc(0)) {
#ifdef vax
		expand(clrnd((int)btoc(szicode)), 0);
		(void) swpexpand(u.u_dsize, 0, &u.u_dmap, &u.u_smap);
		(void) copyout((caddr_t)icode, (caddr_t)0, (unsigned)szicode);
#endif
#ifdef sun
		icode();
		usetup();
		regs.r_context = u.u_pcb.pcb_ctx->ctx_context;
#endif
		/*
		 * Return goes to loc. 0 of user init
		 * code just copied out.
		 */
		return;
	}
#ifdef MUSH
	/*
	 * start MUSH daemon
	 *			pid == 3
	 */
	if (newproc(0)) {
#ifdef vax
		expand(clrnd((int)btoc(szmcode)), 0);
		(void) swpexpand(u.u_dsize, 0, &u.u_dmap, &u.u_smap);
		(void) copyout((caddr_t)mcode, (caddr_t)0, (unsigned)szmcode);
		/*
		 * Return goes to loc. 0 of user mush
		 * code just copied out.
		 */
		return;
#endif
	}
#endif
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
#ifndef sun
		bp->b_un.b_addr = buffers + i * MAXBSIZE;
		if (i < residual)
			bp->b_bufsize = (base + 1) * CLBYTES;
		else
			bp->b_bufsize = base * CLBYTES;
		binshash(bp, &bfreelist[BQ_AGE]);
#else
		bp->b_un.b_addr = (char *)0;
		bp->b_bufsize = 0;
		binshash(bp, &bfreelist[BQ_EMPTY]);
#endif
		bp->b_flags = B_BUSY|B_INVAL;
		brelse(bp);
	}
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

	ccp = (int)cfree;
	ccp = (ccp+CROUND) & ~CROUND;
	for(cp=(struct cblock *)ccp; cp < &cfree[nclist-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
		cfreecount += CBSIZE;
	}
}
