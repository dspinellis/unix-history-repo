/*
 * Copyright (c) 1982,1986,1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)machdep.c	7.14 (Berkeley) 5/26/88
 */

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "malloc.h"
#include "map.h"
#include "vm.h"
#include "proc.h"
#include "buf.h"
#include "reboot.h"
#include "conf.h"
#include "inode.h"
#include "file.h"
#include "text.h"
#include "clist.h"
#include "callout.h"
#include "cmap.h"
#include "mbuf.h"
#include "msgbuf.h"
#include "quota.h"

#include "reg.h"
#include "pte.h"
#include "psl.h"
#include "frame.h"
#include "clock.h"
#include "cons.h"
#include "cpu.h"
#include "mem.h"
#include "mtpr.h"
#include "rpb.h"
#include "ka630.h"

#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"

/*
 * Declare these as initialized data so we can patch them.
 */
int	nswbuf = 0;
#ifdef	NBUF
int	nbuf = NBUF;
#else
int	nbuf = 0;
#endif
#ifdef	BUFPAGES
int	bufpages = BUFPAGES;
#else
int	bufpages = 0;
#endif
int	msgbufmapped;		/* set when safe to use msgbuf */

/*
 * Machine-dependent startup code
 */
startup(firstaddr)
	int firstaddr;
{
	register int unixsize;
	register unsigned i;
	register struct pte *pte;
	int mapaddr, j, n;
	register caddr_t v;
	int maxbufs, base, residual;

#if VAX630
	/*
 	 * Leave last 5k of phys. memory as console work area.
	 */
	if (cpu == VAX_630)
		maxmem -= 10;
#endif
	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxmem -= btoc(sizeof (struct msgbuf));
	pte = msgbufmap;
	for (i = 0; i < btoc(sizeof (struct msgbuf)); i++)
		*(int *)pte++ = PG_V | PG_KW | (maxmem + i);
	mtpr(TBIA, 0);
	msgbufmapped = 1;

#if VAX630
#include "qv.h"
#if NQV > 0
	/*
	 * redirect console to qvss if it exists
	 */
	qvcons_init();
#endif 
#include "qd.h"
#if NQD > 0
	/*
	 * redirect console to qdss if it exists
	 */
	qdcons_init();
#endif
#endif

#ifdef KADB
	kdb_init();
#endif
	/*
	 * Good {morning,afternoon,evening,night}.
	 */
	printf(version);
	printf("real mem  = %d\n", ctob(physmem));

	/*
	 * Allocate space for system data structures.
	 * The first available real memory address is in "firstaddr".
	 * The first available kernel virtual address is in "v".
	 * As pages of kernel virtual memory are allocated, "v" is incremented.
	 * As pages of memory are allocated and cleared,
	 * "firstaddr" is incremented.
	 * An index into the kernel page table corresponding to the
	 * virtual memory address maintained in "v" is kept in "mapaddr".
	 */
	v = (caddr_t)(KERNBASE | (firstaddr * NBPG));
#define	valloc(name, type, num) \
	    (name) = (type *)v; v = (caddr_t)((name)+(num))
#define	valloclim(name, type, num, lim) \
	    (name) = (type *)v; v = (caddr_t)((lim) = ((name)+(num)))
	valloclim(inode, struct inode, ninode, inodeNINODE);
	valloclim(file, struct file, nfile, fileNFILE);
	valloclim(proc, struct proc, nproc, procNPROC);
	valloclim(text, struct text, ntext, textNTEXT);
	valloc(cfree, struct cblock, nclist);
	valloc(callout, struct callout, ncallout);
	valloc(swapmap, struct map, nswapmap = nproc * 2);
	valloc(argmap, struct map, ARGMAPSIZE);
	valloc(kernelmap, struct map, nproc);
	valloc(mbmap, struct map, nmbclusters/4);
	valloc(namecache, struct namecache, nchsize);
	valloc(kmemmap, struct map, ekmempt - kmempt);
	valloc(kmemusage, struct kmemusage, ekmempt - kmempt);
#ifdef QUOTA
	valloclim(quota, struct quota, nquota, quotaNQUOTA);
	valloclim(dquot, struct dquot, ndquot, dquotNDQUOT);
#endif
	
	/*
	 * Determine how many buffers to allocate.
	 * Use 10% of memory for the first 2 Meg, 5% of the remaining
	 * memory. Insure a minimum of 16 buffers.
	 * We allocate 1/2 as many swap buffer headers as file i/o buffers.
	 */
	if (bufpages == 0)
		if (physmem < (2 * 1024 * CLSIZE))
			bufpages = physmem / 10 / CLSIZE;
		else
			bufpages = ((2 * 1024 * CLSIZE + physmem) / 20) / CLSIZE;
	if (nbuf == 0) {
		nbuf = bufpages / 2;
		if (nbuf < 16)
			nbuf = 16;
	}
	if (nswbuf == 0) {
		nswbuf = (nbuf / 2) &~ 1;	/* force even */
		if (nswbuf > 256)
			nswbuf = 256;		/* sanity */
	}
	valloc(swbuf, struct buf, nswbuf);

	/*
	 * Now the amount of virtual memory remaining for buffers
	 * can be calculated, estimating needs for the cmap.
	 */
	ncmap = (maxmem*NBPG - ((int)v &~ KERNBASE)) /
		(CLBYTES + sizeof(struct cmap)) + 2;
	maxbufs = ((SYSPTSIZE * NBPG) -
	    ((int)(v + ncmap * sizeof(struct cmap)) - KERNBASE)) /
		(MAXBSIZE + sizeof(struct buf));
	if (maxbufs < 16)
		panic("sys pt too small");
	if (nbuf > maxbufs) {
		printf("SYSPTSIZE limits number of buffers to %d\n", maxbufs);
		nbuf = maxbufs;
	}
	if (bufpages > nbuf * (MAXBSIZE / CLBYTES))
		bufpages = nbuf * (MAXBSIZE / CLBYTES);
	valloc(buf, struct buf, nbuf);

	/*
	 * Allocate space for core map.
	 * Allow space for all of phsical memory minus the amount 
	 * dedicated to the system. The amount of physical memory
	 * dedicated to the system is the total virtual memory of
	 * the system thus far, plus core map, buffer pages,
	 * and buffer headers not yet allocated.
	 * Add 2: 1 because the 0th entry is unused, 1 for rounding.
	 */
	ncmap = (maxmem*NBPG - ((int)(v + bufpages*CLBYTES) &~ KERNBASE)) /
		(CLBYTES + sizeof(struct cmap)) + 2;
	valloclim(cmap, struct cmap, ncmap, ecmap);

	/*
	 * Clear space allocated thus far, and make r/w entries
	 * for the space in the kernel map.
	 */
	unixsize = btoc((int)v &~ KERNBASE);
	while (firstaddr < unixsize) {
		*(int *)(&Sysmap[firstaddr]) = PG_V | PG_KW | firstaddr;
		clearseg((unsigned)firstaddr);
		firstaddr++;
	}

	/*
	 * Now allocate buffers proper.  They are different than the above
	 * in that they usually occupy more virtual memory than physical.
	 */
	v = (caddr_t) ((int)(v + PGOFSET) &~ PGOFSET);
	valloc(buffers, char, MAXBSIZE * nbuf);
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	mapaddr = firstaddr;
	for (i = 0; i < nbuf; i++) {
		n = (i < residual ? base + 1 : base) * CLSIZE;
		for (j = 0; j < n; j++) {
			*(int *)(&Sysmap[mapaddr+j]) = PG_V | PG_KW | firstaddr;
			clearseg((unsigned)firstaddr);
			firstaddr++;
		}
		mapaddr += MAXBSIZE / NBPG;
	}

	unixsize = btoc((int)v &~ KERNBASE);
	if (firstaddr >= physmem - 8*UPAGES)
		panic("no memory");
	mtpr(TBIA, 0);			/* After we just cleared it all! */

	/*
	 * Initialize callouts
	 */
	callfree = callout;
	for (i = 1; i < ncallout; i++)
		callout[i-1].c_next = &callout[i];

	/*
	 * Initialize memory allocator and swap
	 * and user page table maps.
	 *
	 * THE USER PAGE TABLE MAP IS CALLED ``kernelmap''
	 * WHICH IS A VERY UNDESCRIPTIVE AND INCONSISTENT NAME.
	 */
	meminit(firstaddr, maxmem);
	maxmem = freemem;
	printf("avail mem = %d\n", ctob(maxmem));
	printf("using %d buffers containing %d bytes of memory\n",
		nbuf, bufpages * CLBYTES);
	rminit(kernelmap, (long)USRPTSIZE, (long)1,
	    "usrpt", nproc);
	rminit(mbmap, (long)(nmbclusters * CLSIZE), (long)CLSIZE,
	    "mbclusters", nmbclusters/4);
	kmeminit();	/* now safe to do malloc/free */

	/*
	 * Set up CPU-specific registers, cache, etc.
	 */
	initcpu();

	/*
	 * Set up buffers, so they can be used to read disk labels.
	 */
	bhinit();
	binit();

	/*
	 * Configure the system.
	 */
	configure();

	/*
	 * Clear restart inhibit flags.
	 */
	tocons(TXDB_CWSI);
	tocons(TXDB_CCSI);
}

#ifdef PGINPROF
/*
 * Return the difference (in microseconds)
 * between the  current time and a previous
 * time as represented  by the arguments.
 * If there is a pending clock interrupt
 * which has not been serviced due to high
 * ipl, return error code.
 */
vmtime(otime, olbolt, oicr)
	register int otime, olbolt, oicr;
{

	if (mfpr(ICCS)&ICCS_INT)
		return(-1);
	else
		return(((time.tv_sec-otime)*60 + lbolt-olbolt)*16667 + mfpr(ICR)-oicr);
}
#endif

/*
 * Clear registers on exec
 */
setregs(entry)
	u_long entry;
{
#ifdef notdef
	register int *rp;

	/* should pass args to init on the stack */
	/* should also fix this code before using it, it's wrong */
	/* wanna clear the scb? */
	for (rp = &u.u_ar0[0]; rp < &u.u_ar0[16];)
		*rp++ = 0;
#endif
	u.u_ar0[PC] = entry + 2;
}

/*
 * Send an interrupt to process.
 *
 * Stack is set up to allow sigcode stored
 * in u. to call routine, followed by chmk
 * to sigreturn routine below.  After sigreturn
 * resets the signal mask, the stack, the frame 
 * pointer, and the argument pointer, it returns
 * to the user specified pc, psl.
 */
sendsig(p, sig, mask)
	int (*p)(), sig, mask;
{
	register struct sigcontext *scp;
	register int *regs;
	register struct sigframe {
		int	sf_signum;
		int	sf_code;
		struct	sigcontext *sf_scp;
		int	(*sf_handler)();
		int	sf_argcount;
		struct	sigcontext *sf_scpcopy;
	} *fp;
	int oonstack;

	regs = u.u_ar0;
	oonstack = u.u_onstack;
	/*
	 * Allocate and validate space for the signal handler
	 * context. Note that if the stack is in P0 space, the
	 * call to grow() is a nop, and the useracc() check
	 * will fail if the process has not already allocated
	 * the space with a `brk'.
	 */
	if (!u.u_onstack && (u.u_sigonstack & sigmask(sig))) {
		scp = (struct sigcontext *)u.u_sigsp - 1;
		u.u_onstack = 1;
	} else
		scp = (struct sigcontext *)regs[SP] - 1;
	fp = (struct sigframe *)scp - 1;
	if ((int)fp <= USRSTACK - ctob(u.u_ssize)) 
		(void)grow((unsigned)fp);
	if (useracc((caddr_t)fp, sizeof (*fp) + sizeof (*scp), B_WRITE) == 0) {
		/*
		 * Process has trashed its stack; give it an illegal
		 * instruction to halt it in its tracks.
		 */
		u.u_signal[SIGILL] = SIG_DFL;
		sig = sigmask(SIGILL);
		u.u_procp->p_sigignore &= ~sig;
		u.u_procp->p_sigcatch &= ~sig;
		u.u_procp->p_sigmask &= ~sig;
		psignal(u.u_procp, SIGILL);
		return;
	}
	/* 
	 * Build the argument list for the signal handler.
	 */
	fp->sf_signum = sig;
	if (sig == SIGILL || sig == SIGFPE) {
		fp->sf_code = u.u_code;
		u.u_code = 0;
	} else
		fp->sf_code = 0;
	fp->sf_scp = scp;
	fp->sf_handler = p;
	/*
	 * Build the calls argument frame to be used to call sigreturn
	 */
	fp->sf_argcount = 1;
	fp->sf_scpcopy = scp;
	/*
	 * Build the signal context to be used by sigreturn.
	 */
	scp->sc_onstack = oonstack;
	scp->sc_mask = mask;
	scp->sc_sp = regs[SP];
	scp->sc_fp = regs[FP];
	scp->sc_ap = regs[AP];
	scp->sc_pc = regs[PC];
	scp->sc_ps = regs[PS];
	regs[SP] = (int)fp;
	regs[PS] &= ~(PSL_CM|PSL_FPD);
	regs[PC] = (int)u.u_pcb.pcb_sigc;
	return;
}

/*
 * System call to cleanup state after a signal
 * has been taken.  Reset signal mask and
 * stack state from context left by sendsig (above).
 * Return to previous pc and psl as specified by
 * context left by sendsig. Check carefully to
 * make sure that the user has not modified the
 * psl to gain improper priviledges or to cause
 * a machine fault.
 */
sigreturn()
{
	struct a {
		struct sigcontext *sigcntxp;
	};
	register struct sigcontext *scp;
	register int *regs = u.u_ar0;

	scp = ((struct a *)(u.u_ap))->sigcntxp;
	if (useracc((caddr_t)scp, sizeof (*scp), B_WRITE) == 0)
		return;
	if ((scp->sc_ps & (PSL_MBZ|PSL_IPL|PSL_IS)) != 0 ||
	    (scp->sc_ps & (PSL_PRVMOD|PSL_CURMOD)) != (PSL_PRVMOD|PSL_CURMOD) ||
	    ((scp->sc_ps & PSL_CM) &&
	     (scp->sc_ps & (PSL_FPD|PSL_DV|PSL_FU|PSL_IV)) != 0)) {
		u.u_error = EINVAL;
		return;
	}
	u.u_eosys = JUSTRETURN;
	u.u_onstack = scp->sc_onstack & 01;
	u.u_procp->p_sigmask = scp->sc_mask &~
	    (sigmask(SIGKILL)|sigmask(SIGCONT)|sigmask(SIGSTOP));
	regs[FP] = scp->sc_fp;
	regs[AP] = scp->sc_ap;
	regs[SP] = scp->sc_sp;
	regs[PC] = scp->sc_pc;
	regs[PS] = scp->sc_ps;
}

/* XXX - BEGIN 4.2 COMPATIBILITY */
/*
 * Compatibility with 4.2 chmk $139 used by longjmp()
 */
osigcleanup()
{
	register struct sigcontext *scp;
	register int *regs = u.u_ar0;

	scp = (struct sigcontext *)fuword((caddr_t)regs[SP]);
	if ((int)scp == -1)
		return;
	if (useracc((caddr_t)scp, 3 * sizeof (int), B_WRITE) == 0)
		return;
	u.u_onstack = scp->sc_onstack & 01;
	u.u_procp->p_sigmask = scp->sc_mask &~
	    (sigmask(SIGKILL)|sigmask(SIGCONT)|sigmask(SIGSTOP));
	regs[SP] = scp->sc_sp;
}
/* XXX - END 4.2 COMPATIBILITY */

#ifdef notdef
dorti()
{
	struct frame frame;
	register int sp;
	register int reg, mask;
	extern int ipcreg[];

	(void) copyin((caddr_t)u.u_ar0[FP], (caddr_t)&frame, sizeof (frame));
	sp = u.u_ar0[FP] + sizeof (frame);
	u.u_ar0[PC] = frame.fr_savpc;
	u.u_ar0[FP] = frame.fr_savfp;
	u.u_ar0[AP] = frame.fr_savap;
	mask = frame.fr_mask;
	for (reg = 0; reg <= 11; reg++) {
		if (mask&1) {
			u.u_ar0[ipcreg[reg]] = fuword((caddr_t)sp);
			sp += 4;
		}
		mask >>= 1;
	}
	sp += frame.fr_spa;
	u.u_ar0[PS] = (u.u_ar0[PS] & 0xffff0000) | frame.fr_psw;
	if (frame.fr_s)
		sp += 4 + 4 * (fuword((caddr_t)sp) & 0xff);
	/* phew, now the rei */
	u.u_ar0[PC] = fuword((caddr_t)sp);
	sp += 4;
	u.u_ar0[PS] = fuword((caddr_t)sp);
	sp += 4;
	u.u_ar0[PS] |= PSL_USERSET;
	u.u_ar0[PS] &= ~PSL_USERCLR;
	if (u.u_ar0[PS] & PSL_CM)
		u.u_ar0[PS] &= ~PSL_CM_CLR;
	u.u_ar0[SP] = (int)sp;
}
#endif

/*
 * Memenable enables memory controller corrected data reporting.
 * This runs at regular intervals, turning on the interrupt.
 * The interrupt is turned off, per memory controller, when error
 * reporting occurs.  Thus we report at most once per memintvl.
 */
int	memintvl = MEMINTVL;

memenable()
{

	(*cpuops->cpu_memenable)();
	if (memintvl > 0)
		timeout(memenable, (caddr_t)0, memintvl*hz);
}

/*
 * Memerr is the interrupt routine for corrected read data
 * interrupts.  It looks to see which memory controllers have
 * unreported errors, reports them, and disables further
 * reporting for a time on those controller.
 */
memerr()
{

	(*cpuops->cpu_memerr)();
}

/*
 * Invalidate single all pte's in a cluster
 */
tbiscl(v)
	unsigned v;
{
	register caddr_t addr;		/* must be first reg var */
	register int i;

	asm(".set TBIS,58");
	addr = ptob(v);
	for (i = 0; i < CLSIZE; i++) {
#ifdef lint
		mtpr(TBIS, addr);
#else
		asm("mtpr r11,$TBIS");
#endif
		addr += NBPG;
	}
}
  
int	waittime = -1;

boot(howto)
	register int howto;		/* r11 == how to boot */
{
	register int devtype;		/* r10 == major of root dev */
	extern char *panicstr;

	if ((howto&RB_NOSYNC)==0 && waittime < 0 && bfreelist[0].b_forw) {
		register struct buf *bp;
		int iter, nbusy;

		waittime = 0;
		(void) splnet();
		printf("syncing disks... ");
		/*
		 * Release inodes held by texts before update.
		 */
		if (panicstr == 0)
			xumount(NODEV);
		update();

		for (iter = 0; iter < 20; iter++) {
			nbusy = 0;
			for (bp = &buf[nbuf]; --bp >= buf; )
				if ((bp->b_flags & (B_BUSY|B_INVAL)) == B_BUSY)
					nbusy++;
			if (nbusy == 0)
				break;
			printf("%d ", nbusy);
			DELAY(40000 * iter);
		}
		if (nbusy)
			printf("giving up\n");
		else
			printf("done\n");
		/*
		 * If we've been adjusting the clock, the todr
		 * will be out of synch; adjust it now.
		 */
		resettodr();
	}
	splx(0x1f);			/* extreme priority */
	devtype = major(rootdev);
	if (howto&RB_HALT) {
		/* 630 can be told to halt, but how? */
		printf("halting (in tight loop); hit\n\t^P\n\tHALT\n\n");
		for (;;)
			;
	} else {
		if (howto & RB_DUMP)
			doadump();
		vaxboot();
	}
#ifdef lint
	devtype = devtype;
#endif
	/*NOTREACHED*/
}

/*
 * Reboot after panic or via reboot system call.  Note that r11
 * and r10 must already have the proper boot values (`call by voodoo').
 */
vaxboot()
{

	switch (cpu) {

#ifdef VAX8200
	case VAX_8200:
		/*
		 * TXDB_BOOT erases memory!  Instead we set the `did
		 * a dump' flag in the rpb.
		 */
		*(int *)&Sysmap[0] &= ~PG_PROT;
		*(int *)&Sysmap[0] |= PG_KW;
		mtpr(TBIS, &rpb);
		rpb.rp_flag = 1;
		break;
#endif

	default:
		tocons(TXDB_BOOT);
	}

	/*
	 * Except on 780s and 8600s, boot flags go in r5.  SBI
	 * VAXen do not care, so copy boot flags to r5 always.
	 */
	asm("movl r11,r5");
	for (;;) {
		asm("halt");
	}
}

tocons(c)
{
	register int oldmask;

	while (((oldmask = mfpr(TXCS)) & TXCS_RDY) == 0)
		continue;

	switch (cpu) {

#if VAX8200 || VAX780 || VAX750 || VAX730 || VAX630
	case VAX_8200:
	case VAX_780:
	case VAX_750:
	case VAX_730:
	case VAX_630:
		c |= TXDB_CONS;
		break;
#endif

#if VAX8600
	case VAX_8600:
		mtpr(TXCS, TXCS_LCONS | TXCS_WMASK);
		while ((mfpr(TXCS) & TXCS_RDY) == 0)
			continue;
		break;
#endif
	}

	mtpr(TXDB, c);

#if VAX8600
	switch (cpu) {

	case VAX_8600:
		while ((mfpr(TXCS) & TXCS_RDY) == 0)
			continue;
		mtpr(TXCS, oldmask | TXCS_WMASK);
		break;
	}
#endif
#ifdef lint
	oldmask = oldmask;
#endif
}

int	dumpmag = 0x8fca0101;	/* magic number for savecore */
int	dumpsize = 0;		/* also for savecore */
/*
 * Doadump comes here after turning off memory management and
 * getting on the dump stack, either when called above, or by
 * the auto-restart code.
 */
dumpsys()
{

	rpb.rp_flag = 1;
	msgbufmapped = 0;
	if (dumpdev == NODEV)
		return;
	/*
	 * For dumps during autoconfiguration,
	 * if dump device has already configured...
	 */
	if (dumplo == 0 && bdevsw[major(dumpdev)].d_psize)
		dumplo = (*bdevsw[major(dumpdev)].d_psize)(dumpdev) - physmem;
	if (dumplo < 0)
		dumplo = 0;
	dumpsize = physmem;
	printf("\ndumping to dev %x, offset %d\n", dumpdev, dumplo);
	printf("dump ");
	switch ((*bdevsw[major(dumpdev)].d_dump)(dumpdev)) {

	case ENXIO:
		printf("device bad\n");
		break;

	case EFAULT:
		printf("device not ready\n");
		break;

	case EINVAL:
		printf("area improper\n");
		break;

	case EIO:
		printf("i/o error");
		break;

	default:
		printf("succeeded");
		break;
	}
}

/*
 * Machine check error recovery code.
 */
machinecheck(cmcf)
	caddr_t cmcf;
{

	if ((*cpuops->cpu_mchk)(cmcf) == MCHK_RECOVERED)
		return;
	(*cpuops->cpu_memerr)();
	panic("mchk");
}

#if defined(VAX780) || defined(VAX750)
/*
 * These strings are shared between the 780 and 750 machine check code
 * in ka780.c and ka730.c.
 */
char *mc780750[16] = {
	"cp read",	"ctrl str par",	"cp tbuf par",	"cp cache par",
	"cp rdtimo", 	"cp rds",	"ucode lost",	0,
	0,		0,		"ib tbuf par",	0,
	"ib rds",	"ib rd timo",	0,		"ib cache par"
};
#endif

/*
 * Return the best possible estimate of the time in the timeval
 * to which tvp points.  We do this by reading the interval count
 * register to determine the time remaining to the next clock tick.
 * We must compensate for wraparound which is not yet reflected in the time
 * (which happens when the ICR hits 0 and wraps after the splhigh(),
 * but before the mfpr(ICR)).  Also check that this time is no less than
 * any previously-reported time, which could happen around the time
 * of a clock adjustment.  Just for fun, we guarantee that the time
 * will be greater than the value obtained by a previous call.
 */
microtime(tvp)
	register struct timeval *tvp;
{
	int s = splhigh();
	static struct timeval lasttime;
	register long t;

	*tvp = time;
	t =  mfpr(ICR);
	if (t < -tick / 2 && (mfpr(ICCS) & ICCS_INT))
		t += tick;
	tvp->tv_usec += tick + t;
	if (tvp->tv_usec > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
	if (tvp->tv_sec == lasttime.tv_sec &&
	    tvp->tv_usec <= lasttime.tv_usec &&
	    (tvp->tv_usec = lasttime.tv_usec + 1) > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
	lasttime = *tvp;
	splx(s);
}

initcpu()
{
	/*
	 * Enable cache.
	 */
	switch (cpu) {

#if VAX8600
	case VAX_8600:
		mtpr(CSWP, 3);
		break;
#endif
#if VAX8200
	case VAX_8200:
		mtpr(CADR, 0);
		break;
#endif
#if VAX780
	case VAX_780:
		mtpr(SBIMT, 0x200000);
		break;
#endif
#if VAX750
	case VAX_750:
		mtpr(CADR, 0);
		break;
#endif
	default:
		break;
	}

	/*
	 * Enable floating point accelerator if it exists
	 * and has control register.
	 */
	switch(cpu) {

#if VAX8600 || VAX780
	case VAX_8600:
	case VAX_780:
		if ((mfpr(ACCS) & 0xff) != 0) {
			printf("Enabling FPA\n");
			mtpr(ACCS, 0x8000);
		}
#endif
	default:
		break;
	}
}

/*
 * Return a reasonable approximation of the time of day register.
 * More precisely, return a number that increases by one about
 * once every ten milliseconds.
 */
todr()
{

	switch (cpu) {

#if VAX8600 || VAX8200 || VAX780 || VAX750 || VAX730
	case VAX_8600:
	case VAX_8200:
	case VAX_780:
	case VAX_750:
	case VAX_730:
		return (mfpr(TODR));
#endif

#if VAX630
	case VAX_630:
		/* XXX crude */
		{ static int t; DELAY(10000); return (++t); }
#endif

	default:
		panic("todr");
	}
	/* NOTREACHED */
}
