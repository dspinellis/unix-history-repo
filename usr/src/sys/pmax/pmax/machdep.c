/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: machdep.c 1.63 91/04/24$
 *
 *	@(#)machdep.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "signalvar.h"
#include "kernel.h"
#include "map.h"
#include "proc.h"
#include "buf.h"
#include "reboot.h"
#include "conf.h"
#include "file.h"
#include "clist.h"
#include "callout.h"
#include "malloc.h"
#include "mbuf.h"
#include "msgbuf.h"
#include "user.h"
#ifdef SYSVSHM
#include "shm.h"
#endif

#include "vm/vm.h"
#include "vm/vm_kern.h"
#include "vm/vm_page.h"

#include "../include/cpu.h"
#include "../include/reg.h"
#include "../include/psl.h"
#include "../include/machMon.h"
#include "clockreg.h"
#include "pte.h"

#define	MAXMEM	24*1024*CLSIZE	/* XXX - from cmap.h */

vm_map_t buffer_map;

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
int	maxmem;			/* max memory per process */
int	physmem = MAXMEM;	/* max supported memory, changes to actual */
/*
 * safepri is a safe priority for sleep to set for a spin-wait
 * during autoconfiguration or after a panic.
 */
int	safepri = PSL_LOWIPL;

struct	user *proc0paddr;
struct	proc nullproc;		/* for use by swtch_exit() */

/*
 * Do all the stuff that locore normally does before calling main().
 * Process arguments passed to us by the prom monitor.
 * Return the first page address following the system.
 */
mach_init(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;
	register int i;
	register unsigned firstaddr;
	register caddr_t v;
	extern char edata[], end[];
	extern char MachUTLBMiss[], MachUTLBMissEnd[];
	extern char MachException[], MachExceptionEnd[];
#ifdef ATTR
	extern char *pmap_attributes;
#endif

	/* clear BSS segment, pages for u, and proc[0] page table */
	v = (caddr_t)pmax_round_page(end) + 2 * UPAGES * NBPG;
	bzero(edata, v - edata);

	/* look at argv[0] and compute bootdev */
	makebootdev(argv[0]);

	/*
	 * Look at arguments passed to us and compute boothowto.
	 */
#ifdef GENERIC
	boothowto = RB_SINGLE | RB_ASKNAME;
#else
	boothowto = RB_SINGLE | RB_DFLTROOT;
#endif
	if (argc > 1) {
		for (i = 1; i < argc; i++) {
			for (cp = argv[i]; *cp; cp++) {
				switch (*cp) {
				case '-':
					continue;

				case '0': /* XXX */
				case '1': /* XXX */
				case '2': /* XXX */
				case '3': /* XXX */
				case '4': /* XXX */
				case '5': /* XXX */
				case '6': /* XXX */
				    {
					extern int sii_debug;

					sii_debug = *cp - '0';
					break;
				    }

				case 'a': /* autoboot */
					boothowto &= ~RB_SINGLE;
					break;

				case 'n': /* ask for names */
					boothowto |= RB_ASKNAME;
					boothowto &= ~RB_DFLTROOT;
					break;

				case 'N': /* don't ask for names */
					boothowto &= ~RB_ASKNAME;
					boothowto |= RB_DFLTROOT;
				}
			}
		}
	}

	/*
	 * Init mapping for u page(s) for proc[0], pm_tlbpid 1.
	 */
	firstaddr = pmax_round_page(end);
	curproc->p_addr = proc0paddr = (struct user *)firstaddr;
	curproc->p_regs = proc0paddr->u_pcb.pcb_regs;
	for (i = 0; i < UPAGES; i++) {
		MachTLBWriteIndexed(i,
			(UADDR + (i << PGSHIFT)) | (1 << VMMACH_TLB_PID_SHIFT),
			curproc->p_md.md_upte[i] = firstaddr | PG_V | PG_M);
		firstaddr += NBPG;
	}
	MachSetPID(1);

	/*
	 * init nullproc for swtch_exit().
	 * init mapping for u page(s), pm_tlbpid 0
	 * This could be used for an idle process.
	 */
	nullproc.p_regs = ((struct user *)firstaddr)->u_pcb.pcb_regs;
	for (i = 0; i < UPAGES; i++) {
		nullproc.p_md.md_upte[i] = firstaddr | PG_V | PG_M;
		firstaddr += NBPG;
	}

	/*
	 * Copy down exception vector code.
	 */
	if (MachUTLBMissEnd - MachUTLBMiss > 0x80)
		panic("startup: UTLB code too large");
	bcopy(MachUTLBMiss, (char *)MACH_UTLB_MISS_EXC_VEC,
		MachUTLBMissEnd - MachUTLBMiss);
	bcopy(MachException, (char *)MACH_GEN_EXC_VEC,
		MachExceptionEnd - MachException);

	/*
	 * Clear out the I and D caches.
	 */
	MachConfigCache();
	MachFlushCache();

	/*
	 * Find out how much memory is available.
	 */
	physmem = btoc(v - KERNBASE);
	cp = (char *)(MACH_UNCACHED_MEMORY_ADDR + (physmem << PGSHIFT));
	while (cp < (char *)MACH_UNCACHED_FRAME_BUFFER_ADDR) {
		if (badaddr(cp, 4))
			break;
		cp += NBPG;
		physmem++;
	}
	maxmem = physmem + btoc(KERNBASE);

	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxmem -= btoc(sizeof (struct msgbuf));
	msgbufp = (struct msgbuf *)(maxmem << PGSHIFT);
	msgbufmapped = 1;

	/*
	 * Allocate space for system data structures.
	 * The first available real memory address is in "firstaddr".
	 * The first available kernel virtual address is in "v".
	 * As pages of kernel virtual memory are allocated, "v" is incremented.
	 * As pages of memory are allocated and cleared,
	 * "firstaddr" is incremented.
	 */
	/*
	 * These data structures are allocated here instead of cpu_startup()
	 * because physical memory is directly addressable. We don't have
	 * to map these into virtual address space.
	 */
	firstaddr = (unsigned)v;

#define	valloc(name, type, num) \
	    (name) = (type *)v; v = (caddr_t)((name)+(num))
#define	valloclim(name, type, num, lim) \
	    (name) = (type *)v; v = (caddr_t)((lim) = ((name)+(num)))
	valloc(cfree, struct cblock, nclist);
	valloc(callout, struct callout, ncallout);
	valloc(swapmap, struct map, nswapmap = maxproc * 2);
#ifdef SYSVSHM
	valloc(shmsegs, struct shmid_ds, shminfo.shmmni);
#endif
#ifdef ATTR
	/* this is allocated here just to save a few bytes */
	valloc(pmap_attributes, char, physmem);
#endif

	/*
	 * Determine how many buffers to allocate.
	 * We allocate more buffer space than the BSD standard of
	 * using 10% of memory for the first 2 Meg, 5% of remaining.
	 * We just allocate a flat 10%.  Insure a minimum of 16 buffers.
	 * We allocate 1/2 as many swap buffer headers as file i/o buffers.
	 */
	if (bufpages == 0)
		bufpages = physmem / 10 / CLSIZE;
	if (nbuf == 0) {
		nbuf = bufpages;
		if (nbuf < 16)
			nbuf = 16;
	}
	if (nswbuf == 0) {
		nswbuf = (nbuf / 2) &~ 1;	/* force even */
		if (nswbuf > 256)
			nswbuf = 256;		/* sanity */
	}
	valloc(swbuf, struct buf, nswbuf);
	valloc(buf, struct buf, nbuf);

	/*
	 * Clear allocated memory.
	 */
	v = (caddr_t)pmax_round_page(v);
	bzero((caddr_t)firstaddr, (unsigned)v - firstaddr);

	/*
	 * Initialize the virtual memory system.
	 */
	pmap_bootstrap((vm_offset_t)v);
}

/*
 * Console initialization: called early on from main,
 * before vm init or startup.  Do enough configuration
 * to choose and initialize a console.
 */
consinit()
{

#include "pm.h"
#if NPM > 0
	/*
	 * Initialize the console before we print anything out.
	 */
	pminit();
#endif
}

/*
 * cpu_startup: allocate memory for variable-sized tables,
 * initialize cpu, and do autoconfiguration.
 */
cpu_startup()
{
	register unsigned i;
	register caddr_t v, firstaddr;
	int base, residual;
	extern long Usrptsize;
	extern struct map *useriomap;
#ifdef DEBUG
	extern int pmapdebug;
	int opmapdebug = pmapdebug;
#endif
	vm_offset_t minaddr, maxaddr;
	vm_size_t size;

#ifdef DEBUG
	pmapdebug = 0;
#endif

	/*
	 * Good {morning,afternoon,evening,night}.
	 */
	printf(version);
	printf("real mem = %d\n", ctob(physmem));

	/*
	 * Allocate virtual address space for file I/O buffers.
	 * Note they are different than the array of headers, 'buf',
	 * and usually occupy more virtual memory than physical.
	 */
	size = MAXBSIZE * nbuf;
	buffer_map = kmem_suballoc(kernel_map, (vm_offset_t)&buffers,
				   &maxaddr, size, FALSE);
	minaddr = (vm_offset_t)buffers;
	if (vm_map_find(buffer_map, vm_object_allocate(size), (vm_offset_t)0,
			&minaddr, size, FALSE) != KERN_SUCCESS)
		panic("startup: cannot allocate buffers");
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < nbuf; i++) {
		vm_size_t curbufsize;
		vm_offset_t curbuf;

		/*
		 * First <residual> buffers get (base+1) physical pages
		 * allocated for them.  The rest get (base) physical pages.
		 *
		 * The rest of each buffer occupies virtual space,
		 * but has no physical memory allocated for it.
		 */
		curbuf = (vm_offset_t)buffers + i * MAXBSIZE;
		curbufsize = CLBYTES * (i < residual ? base+1 : base);
		vm_map_pageable(buffer_map, curbuf, curbuf+curbufsize, FALSE);
		vm_map_simplify(buffer_map, curbuf);
	}
	/*
	 * Allocate a submap for exec arguments.  This map effectively
	 * limits the number of processes exec'ing at any time.
	 */
	exec_map = kmem_suballoc(kernel_map, &minaddr, &maxaddr,
				 16*NCARGS, TRUE);
	/*
	 * Allocate a submap for physio
	 */
	phys_map = kmem_suballoc(kernel_map, &minaddr, &maxaddr,
				 VM_PHYS_SIZE, TRUE);

	/*
	 * Finally, allocate mbuf pool.  Since mclrefcnt is an off-size
	 * we use the more space efficient malloc in place of kmem_alloc.
	 */
	mclrefcnt = (char *)malloc(NMBCLUSTERS+CLBYTES/MCLBYTES,
				   M_MBUF, M_NOWAIT);
	bzero(mclrefcnt, NMBCLUSTERS+CLBYTES/MCLBYTES);
	mb_map = kmem_suballoc(kernel_map, (vm_offset_t)&mbutl, &maxaddr,
			       VM_MBUF_SIZE, FALSE);
	/*
	 * Initialize callouts
	 */
	callfree = callout;
	for (i = 1; i < ncallout; i++)
		callout[i-1].c_next = &callout[i];

#ifdef DEBUG
	pmapdebug = opmapdebug;
#endif
	printf("avail mem = %d\n", ptoa(cnt.v_free_count));
	printf("using %d buffers containing %d bytes of memory\n",
		nbuf, bufpages * CLBYTES);
	/*
	 * Set up CPU-specific registers, cache, etc.
	 */
	initcpu();

	/*
	 * Set up buffers, so they can be used to read disk labels.
	 */
	bufinit();

	/*
	 * Configure the system.
	 */
	configure();
}

/*
 * Set registers on exec.
 * Clear all registers except sp, pc.
 */
setregs(p, entry, retval)
	register struct proc *p;
	u_long entry;
	int retval[2];
{
	int sp = p->p_regs[SP];

	bzero((caddr_t)p->p_regs, (FSR + 1) * sizeof(int));
	p->p_regs[SP] = sp;
	p->p_regs[PC] = entry;
	p->p_regs[PS] = PSL_USERSET;
	p->p_md.md_flags & ~MDP_FPUSED;
}

/*
 * WARNING: code in locore.s assumes the layout shown for sf_signum
 * thru sf_handler so... don't screw with them!
 */
struct sigframe {
	int	sf_signum;		/* signo for handler */
	int	sf_code;		/* additional info for handler */
	struct	sigcontext *sf_scp;	/* context ptr for handler */
	sig_t	sf_handler;		/* handler addr for u_sigc */
};

#ifdef DEBUG
int sigdebug = 0;
int sigpid = 0;
#define SDB_FOLLOW	0x01
#define SDB_KSTACK	0x02
#define SDB_FPSTATE	0x04
#endif

/*
 * Send an interrupt to process.
 */
void
sendsig(catcher, sig, mask, code)
	sig_t catcher;
	int sig, mask;
	unsigned code;
{
	register struct proc *p = curproc;
	register struct sigframe *fp;
	register struct sigacts *ps = p->p_sigacts;
	register struct sigcontext *scp;
	register int *regs;
	int oonstack, fsize;
	struct sigcontext ksc;

	regs = p->p_regs;
	oonstack = ps->ps_onstack;
	/*
	 * Allocate and validate space for the signal handler
	 * context. Note that if the stack is in data space, the
	 * call to grow() is a nop, and the copyout()
	 * will fail if the process has not already allocated
	 * the space with a `brk'.
	 */
	if (!ps->ps_onstack && (ps->ps_sigonstack & sigmask(sig))) {
		scp = (struct sigcontext *)ps->ps_sigsp - 1;
		ps->ps_onstack = 1;
	} else
		scp = (struct sigcontext *)regs[SP] - 1;
	fp = (struct sigframe *)scp - 1;
	if ((unsigned)fp <= USRSTACK - ctob(p->p_vmspace->vm_ssize)) 
		(void)grow(p, (unsigned)fp);
	/*
	 * Build the signal context to be used by sigreturn.
	 */
	ksc.sc_onstack = oonstack;
	ksc.sc_mask = mask;
	ksc.sc_pc = regs[PC];
	ksc.sc_regs[ZERO] = 0xACEDBADE;		/* magic number */
	bcopy((caddr_t)&regs[1], (caddr_t)&ksc.sc_regs[1],
		sizeof(ksc.sc_regs) - sizeof(int));
	ksc.sc_fpused = p->p_md.md_flags & MDP_FPUSED;
	if (ksc.sc_fpused) {
		extern struct proc *machFPCurProcPtr;

		/* if FPU has current state, save it first */
		if (p == machFPCurProcPtr) {
			MachSaveCurFPState(p);
			machFPCurProcPtr = (struct proc *)0;
		}
		bcopy((caddr_t)&p->p_regs[F0], (caddr_t)ksc.sc_fpregs,
			sizeof(ksc.sc_fpregs));
	}
	if (copyout((caddr_t)&ksc, (caddr_t)scp, sizeof(ksc))) {
		/*
		 * Process has trashed its stack; give it an illegal
		 * instruction to halt it in its tracks.
		 */
		SIGACTION(p, SIGILL) = SIG_DFL;
		sig = sigmask(SIGILL);
		p->p_sigignore &= ~sig;
		p->p_sigcatch &= ~sig;
		p->p_sigmask &= ~sig;
		psignal(p, SIGILL);
		return;
	}
	/* 
	 * Build the argument list for the signal handler.
	 */
	regs[A0] = sig;
	regs[A1] = code;
	regs[A2] = (int)scp;
	regs[A3] = (int)catcher;

	regs[PC] = (int)catcher;
	regs[SP] = (int)fp;
	regs[RA] = KERNBASE;	/* this causes a trap which we interpret as
				 * meaning "do a sigreturn". */
#ifdef DEBUG
	if ((sigdebug & SDB_KSTACK) && p->p_pid == sigpid)
		printf("sendsig(%d): sig %d ssp %x usp %x scp %x\n",
		       p->p_pid, sig, &oonstack, fp, fp->sf_scp);
#endif
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
/* ARGSUSED */
sigreturn(p, uap, retval)
	struct proc *p;
	struct args {
		struct sigcontext *sigcntxp;
	} *uap;
	int *retval;
{
	register struct sigcontext *scp;
	register int *regs;
	struct sigcontext ksc;
	int error;

	register struct frame *frame;
	register int rf;
	struct sigcontext tsigc;
	int flags;

	scp = uap->sigcntxp;
#ifdef DEBUG
	if (sigdebug & SDB_FOLLOW)
		printf("sigreturn: pid %d, scp %x\n", p->p_pid, scp);
#endif
	regs = p->p_regs;
	/*
	 * Test and fetch the context structure.
	 * We grab it all at once for speed.
	 */
	error = copyin((caddr_t)scp, (caddr_t)&ksc, sizeof(ksc));
	if (error != 0 || ksc.sc_regs[ZERO] != 0xACEDBADE ||
	    (unsigned)ksc.sc_regs[SP] < (unsigned)regs[SP]) {
#ifdef DEBUG
		if (!(sigdebug & SDB_FOLLOW))
			printf("sigreturn: pid %d, scp %x\n", p->p_pid, scp);
		printf("  old sp %x ra %x pc %x\n",
			regs[SP], regs[RA], regs[PC]);
		printf("  new sp %x ra %x pc %x err %d z %x\n",
			ksc.sc_regs[SP], ksc.sc_regs[RA], ksc.sc_regs[PC],
			error, ksc.sc_regs[ZERO]);
#endif
		if (regs[PC] == KERNBASE) {
			int sig;

			/*
			 * Process has trashed its stack; give it an illegal
			 * instruction to halt it in its tracks.
			 */
			SIGACTION(p, SIGILL) = SIG_DFL;
			sig = sigmask(SIGILL);
			p->p_sigignore &= ~sig;
			p->p_sigcatch &= ~sig;
			p->p_sigmask &= ~sig;
			psignal(p, SIGILL);
		}
		return (EINVAL);
	}
	/*
	 * Restore the user supplied information
	 */
	p->p_sigacts->ps_onstack = scp->sc_onstack & 01;
	p->p_sigmask = scp->sc_mask &~ sigcantmask;
	regs[PC] = ksc.sc_pc;
	bcopy((caddr_t)&ksc.sc_regs[1], (caddr_t)&regs[1],
		sizeof(ksc.sc_regs) - sizeof(int));
	ksc.sc_fpused = p->p_md.md_flags & MDP_FPUSED;
	if (ksc.sc_fpused)
		bcopy((caddr_t)ksc.sc_fpregs, (caddr_t)&p->p_regs[F0],
			sizeof(ksc.sc_fpregs));
	return (EJUSTRETURN);
}

int	waittime = -1;

boot(howto)
	register int howto;
{

	trapDump("boot"); /* XXX */
	/* take a snap shot before clobbering any registers */
	if (curproc)
		savectx(curproc->p_addr, 0);

	howto |= RB_HALT; /* XXX */
	boothowto = howto;
	if ((howto&RB_NOSYNC) == 0 && waittime < 0 && bfreelist[0].b_forw) {
		register struct buf *bp;
		int iter, nbusy;

		waittime = 0;
		(void) spl0();
		printf("syncing disks... ");
		/*
		 * Release vnodes held by texts before sync.
		 */
		if (panicstr == 0)
			vnode_pager_umount(NULL);
#ifdef notdef
#include "fd.h"
#if NFD > 0
		fdshutdown();
#endif
#endif
		sync(&proc0, (void *)NULL, (int *)NULL);

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
	(void) splhigh();		/* extreme priority */
	if (howto & RB_HALT) {
#ifdef DEBUG
		void (*f)() = (void (*)())MACH_MON_RESTART;
#else
		void (*f)() = (void (*)())MACH_MON_REINIT;
#endif

		(*f)();	/* jump back to prom monitor */
	} else {
		void (*f)() = (void (*)())MACH_MON_AUTOBOOT;

		if (howto & RB_DUMP)
			dumpsys();
		(*f)();	/* jump back to prom monitor and do 'auto' cmd */
		/*NOTREACHED*/
	}
	/*NOTREACHED*/
}

int	dumpmag = 0x8fca0101;	/* magic number for savecore */
int	dumpsize = 0;		/* also for savecore */
long	dumplo = 0;

dumpconf()
{
	int nblks;

	dumpsize = physmem;
	if (dumpdev != NODEV && bdevsw[major(dumpdev)].d_psize) {
		nblks = (*bdevsw[major(dumpdev)].d_psize)(dumpdev);
		if (dumpsize > btoc(dbtob(nblks - dumplo)))
			dumpsize = btoc(dbtob(nblks - dumplo));
		else if (dumplo == 0)
			dumplo = nblks - btodb(ctob(physmem));
	}
	/*
	 * Don't dump on the first CLBYTES (why CLBYTES?)
	 * in case the dump device includes a disk label.
	 */
	if (dumplo < btodb(CLBYTES))
		dumplo = btodb(CLBYTES);
}

/*
 * Doadump comes here after turning off memory management and
 * getting on the dump stack, either when called above, or by
 * the auto-restart code.
 */
dumpsys()
{
	int error;

	msgbufmapped = 0;
	if (dumpdev == NODEV)
		return;
	/*
	 * For dumps during autoconfiguration,
	 * if dump device has already configured...
	 */
	if (dumpsize == 0)
		dumpconf();
	if (dumplo < 0)
		return;
	printf("\ndumping to dev %x, offset %d\n", dumpdev, dumplo);
	printf("dump ");
	switch (error = (*bdevsw[major(dumpdev)].d_dump)(dumpdev)) {

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
		printf("i/o error\n");
		break;

	default:
		printf("error %d\n", error);
		break;

	case 0:
		printf("succeeded\n");
	}
}

/*
 * Return the best possible estimate of the time in the timeval
 * to which tvp points.  Unfortunately, we can't read the hardware registers.
 * We guarantee that the time will be greater than the value obtained by a
 * previous call.
 */
microtime(tvp)
	register struct timeval *tvp;
{
	int s = splclock();
	static struct timeval lasttime;

	*tvp = time;
#ifdef notdef
	tvp->tv_usec += clkread();
	while (tvp->tv_usec > 1000000) {
		tvp->tv_sec++;
		tvp->tv_usec -= 1000000;
	}
#endif
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
	register volatile struct chiptime *c;
	int i;

	/* disable clock interrupts (until startrtclock()) */
	c = (volatile struct chiptime *)MACH_CLOCK_ADDR;
	c->regb = REGB_DATA_MODE | REGB_HOURS_FORMAT;
	i = c->regc;
	spl0();		/* safe to turn interrupts on now */
	return (i);
}
