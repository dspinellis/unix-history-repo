/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department, The Mach Operating System project at
 * Carnegie-Mellon University and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)machdep.c	7.11 (Berkeley) %G%
 */

/* from: Utah $Hdr: machdep.c 1.63 91/04/24$ */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/signalvar.h>
#include <sys/kernel.h>
#include <sys/map.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/reboot.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/clist.h>
#include <sys/callout.h>
#include <sys/malloc.h>
#include <sys/mbuf.h>
#include <sys/msgbuf.h>
#include <sys/user.h>
#include <sys/exec.h>
#ifdef SYSVSHM
#include <sys/shm.h>
#endif

#include <vm/vm.h>
#include <vm/vm_kern.h>
#include <vm/vm_page.h>

#include <machine/cpu.h>
#include <machine/reg.h>
#include <machine/psl.h>
#include <machine/machMon.h>
#include <machine/pte.h>

#include <pmax/dev/device.h>

#include <pmax/pmax/clockreg.h>

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
int	physmem;		/* max supported memory, changes to actual */
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
	caddr_t start;
	extern char edata[], end[];
	extern char MachUTLBMiss[], MachUTLBMissEnd[];
	extern char MachException[], MachExceptionEnd[];
#ifdef ATTR
	extern char *pmap_attributes;
#endif

	/* clear the BSS segment */
	v = (caddr_t)pmax_round_page(end);
	bzero(edata, v - edata);

#ifdef DS5000
	/* check for direct boot from DS5000 PROM */
	if (argc > 0 && strcmp(argv[0], "boot") == 0) {
		argc--;
		argv++;
	}
#endif

	/* look at argv[0] and compute bootdev */
	makebootdev(argv[0]);

	/*
	 * Look at arguments passed to us and compute boothowto.
	 */
#ifdef GENERIC
	boothowto = RB_SINGLE | RB_ASKNAME;
#else
	boothowto = RB_SINGLE;
#endif
#ifdef KADB
	boothowto |= RB_KDB;
#endif
	if (argc > 1) {
		for (i = 1; i < argc; i++) {
			for (cp = argv[i]; *cp; cp++) {
				switch (*cp) {
				case 'a': /* autoboot */
					boothowto &= ~RB_SINGLE;
					break;

				case 'd': /* use compiled in default root */
					boothowto |= RB_DFLTROOT;
					break;

				case 'm': /* mini root present in memory */
					boothowto |= RB_MINIROOT;
					break;

				case 'n': /* ask for names */
					boothowto |= RB_ASKNAME;
					break;

				case 'N': /* don't ask for names */
					boothowto &= ~RB_ASKNAME;
				}
			}
		}
	}

#ifdef MFS
	/*
	 * Check to see if a mini-root was loaded into memory. It resides
	 * at the start of the next page just after the end of BSS.
	 */
	if (boothowto & RB_MINIROOT)
		v += mfs_initminiroot(v);
#endif

	/*
	 * Init mapping for u page(s) for proc[0], pm_tlbpid 1.
	 */
	start = v;
	curproc->p_addr = proc0paddr = (struct user *)v;
	curproc->p_md.md_regs = proc0paddr->u_pcb.pcb_regs;
	firstaddr = MACH_CACHED_TO_PHYS(v);
	for (i = 0; i < UPAGES; i++) {
		MachTLBWriteIndexed(i,
			(UADDR + (i << PGSHIFT)) | (1 << VMMACH_TLB_PID_SHIFT),
			curproc->p_md.md_upte[i] = firstaddr | PG_V | PG_M);
		firstaddr += NBPG;
	}
	v += UPAGES * NBPG;
	MachSetPID(1);

	/*
	 * init nullproc for swtch_exit().
	 * init mapping for u page(s), pm_tlbpid 0
	 * This could be used for an idle process.
	 */
	nullproc.p_addr = (struct user *)v;
	nullproc.p_md.md_regs = ((struct user *)v)->u_pcb.pcb_regs;
	for (i = 0; i < UPAGES; i++) {
		nullproc.p_md.md_upte[i] = firstaddr | PG_V | PG_M;
		firstaddr += NBPG;
	}
	v += UPAGES * NBPG;

	/* clear pages for u areas */
	bzero(start, v - start);

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
	 * Determine what model of computer we are running on.
	 */
	{
		char *(*f)() = (char *(*)())MACH_MON_GETENV2;

		if (cp = (*f)("systype"))
			i = atoi(cp);
		else
			cp = "";

		/* check for MIPS based platform */
		if (((i >> 24) & 0xFF) != 0x82) {
			printf("Unknown System type '%s'\n", cp);
			boot(RB_HALT | RB_NOSYNC);
		}
	}

	/* check what model platform we are running on */
	switch ((i >> 16) & 0xFF) {
#ifdef DS3100
	case 1:	/* DS3100 Pmax */
		/*
		 * Find out how much memory is available.
		 */
		physmem = btoc(v - KERNBASE);
		cp = (char *)MACH_PHYS_TO_UNCACHED(physmem << PGSHIFT);
		while (cp < (char *)MACH_MAX_MEM_ADDR) {
			if (badaddr(cp, 4))
				break;
			cp += NBPG;
			physmem++;
		}
		break;
#endif

#ifdef DS5000
	case 2:	/* DS5000 3max */
	    {
		extern void tc_find_all_options();

		/* disable all TURBOchannel interrupts */
		i = *(volatile int *)MACH_SYS_CSR_ADDR;
		*(volatile int *)MACH_SYS_CSR_ADDR = i & ~(MACH_CSR_MBZ | 0xFF);

		/*
		 * Probe the TURBOchannel to see what controllers are present.
		 */
		tc_find_all_options();

		/* clear any memory errors from probes */
		*(unsigned *)MACH_ERROR_ADDR = 0;

		/*
		 * Find out how much memory is available.
		 */
		physmem = btoc(v - KERNBASE);
		cp = (char *)MACH_PHYS_TO_UNCACHED(physmem << PGSHIFT);
		while (cp < (char *)MACH_MAX_MEM_ADDR) {
			if (badaddr(cp, 4))
				break;
			*(int *)cp = 0xa5a5a5a5;
			/*
			 * Data will persist on the bus if we read it right
			 * away. Have to be tricky here.
			 */
			((int *)cp)[4] = 0x5a5a5a5a;
			MachEmptyWriteBuffer();
			if (*(int *)cp != 0xa5a5a5a5)
				break;
			cp += NBPG;
			physmem++;
		}
		break;
	    }
#endif DS5000

	case 5:	/* DS5800 Isis */
	case 6:	/* DS5400 MIPSfair */
	default:
		printf("kernel not configured for systype 0x%x\n", i);
		boot(RB_HALT | RB_NOSYNC);
	}

	maxmem = physmem;

	/*
	 * Initialize error message buffer (at end of core).
	 */
	maxmem -= btoc(sizeof (struct msgbuf));
	msgbufp = (struct msgbuf *)(MACH_PHYS_TO_CACHED(maxmem << PGSHIFT));
	msgbufmapped = 1;

	/*
	 * Allocate space for system data structures.
	 * The first available kernel virtual address is in "v".
	 * As pages of kernel virtual memory are allocated, "v" is incremented.
	 *
	 * These data structures are allocated here instead of cpu_startup()
	 * because physical memory is directly addressable. We don't have
	 * to map these into virtual address space.
	 */
	start = v;

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
	bzero(start, v - start);

	/*
	 * Initialize the virtual memory system.
	 */
	pmap_bootstrap((vm_offset_t)MACH_CACHED_TO_PHYS(v));
}

/*
 * Console initialization: called early on from main,
 * before vm init or startup.  Do enough configuration
 * to choose and initialize a console.
 * XXX need something better here.
 */
consinit()
{

#include "pm.h"
#if NPM > 0
	if (pminit())
		return;
#endif

#include "cfb.h"
#if NCFB > 0
	{
		register struct pmax_ctlr *cp;
		register struct driver *drp;

		for (cp = pmax_cinit; drp = cp->pmax_driver; cp++) {
			if (strcmp(drp->d_name, "cfb"))
				continue;
			if (cfb_init(cp))
				return;
		}
	}
#endif
}

/*
 * cpu_startup: allocate memory for variable-sized tables,
 * initialize cpu, and do autoconfiguration.
 */
cpu_startup()
{
	register unsigned i;
	register caddr_t v;
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
	buffer_map = kmem_suballoc(kernel_map, (vm_offset_t *)&buffers,
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
	mb_map = kmem_suballoc(kernel_map, (vm_offset_t *)&mbutl, &maxaddr,
			       VM_MBUF_SIZE, FALSE);
	/*
	 * Initialize callouts
	 */
	callfree = callout;
	for (i = 1; i < ncallout; i++)
		callout[i-1].c_next = &callout[i];
	callout[i-1].c_next = NULL;

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
	int sp = p->p_md.md_regs[SP];
	extern struct proc *machFPCurProcPtr;

	bzero((caddr_t)p->p_md.md_regs, (FSR + 1) * sizeof(int));
	p->p_md.md_regs[SP] = sp;
	p->p_md.md_regs[PC] = entry & ~3;
	p->p_md.md_regs[PS] = PSL_USERSET;
	p->p_md.md_flags & ~MDP_FPUSED;
	if (machFPCurProcPtr == p)
		machFPCurProcPtr = (struct proc *)0;
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
	struct	sigcontext sf_sc;	/* actual context */
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
	register int *regs;
	register struct sigacts *psp = p->p_sigacts;
	int oonstack, fsize;
	struct sigcontext ksc;
	extern char sigcode[], esigcode[];

	regs = p->p_md.md_regs;
	oonstack = psp->ps_sigstk.ss_flags & SA_ONSTACK;
	/*
	 * Allocate and validate space for the signal handler
	 * context. Note that if the stack is in data space, the
	 * call to grow() is a nop, and the copyout()
	 * will fail if the process has not already allocated
	 * the space with a `brk'.
	 */
	fsize = sizeof(struct sigframe);
	if ((psp->ps_flags & SAS_ALTSTACK) &&
	    (psp->ps_sigstk.ss_flags & SA_ONSTACK) == 0 &&
	    (psp->ps_sigonstack & sigmask(sig))) {
		fp = (struct sigframe *)(psp->ps_sigstk.ss_base +
					 psp->ps_sigstk.ss_size - fsize);
		psp->ps_sigstk.ss_flags |= SA_ONSTACK;
	} else
		fp = (struct sigframe *)(regs[SP] - fsize);
	if ((unsigned)fp <= USRSTACK - ctob(p->p_vmspace->vm_ssize)) 
		(void)grow(p, (unsigned)fp);
#ifdef DEBUG
	if ((sigdebug & SDB_FOLLOW) ||
	    (sigdebug & SDB_KSTACK) && p->p_pid == sigpid)
		printf("sendsig(%d): sig %d ssp %x usp %x scp %x\n",
		       p->p_pid, sig, &oonstack, fp, &fp->sf_sc);
#endif
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
		if (p == machFPCurProcPtr)
			MachSaveCurFPState(p);
		bcopy((caddr_t)&p->p_md.md_regs[F0], (caddr_t)ksc.sc_fpregs,
			sizeof(ksc.sc_fpregs));
	}
	if (copyout((caddr_t)&ksc, (caddr_t)&fp->sf_sc, sizeof(ksc))) {
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
	regs[A2] = (int)&fp->sf_sc;
	regs[A3] = (int)catcher;

	regs[PC] = (int)catcher;
	regs[SP] = (int)fp;
	/*
	 * Signal trampoline code is at base of user stack.
	 */
	regs[RA] = (int)PS_STRINGS - (esigcode - sigcode);
#ifdef DEBUG
	if ((sigdebug & SDB_FOLLOW) ||
	    (sigdebug & SDB_KSTACK) && p->p_pid == sigpid)
		printf("sendsig(%d): sig %d returns\n",
		       p->p_pid, sig);
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
struct sigreturn_args {
	struct sigcontext *sigcntxp;
};
/* ARGSUSED */
sigreturn(p, uap, retval)
	struct proc *p;
	struct sigreturn_args *uap;
	int *retval;
{
	register struct sigcontext *scp;
	register int *regs;
	struct sigcontext ksc;
	int error;

	scp = uap->sigcntxp;
#ifdef DEBUG
	if (sigdebug & SDB_FOLLOW)
		printf("sigreturn: pid %d, scp %x\n", p->p_pid, scp);
#endif
	regs = p->p_md.md_regs;
	/*
	 * Test and fetch the context structure.
	 * We grab it all at once for speed.
	 */
	error = copyin((caddr_t)scp, (caddr_t)&ksc, sizeof(ksc));
	if (error || ksc.sc_regs[ZERO] != 0xACEDBADE) {
#ifdef DEBUG
		if (!(sigdebug & SDB_FOLLOW))
			printf("sigreturn: pid %d, scp %x\n", p->p_pid, scp);
		printf("  old sp %x ra %x pc %x\n",
			regs[SP], regs[RA], regs[PC]);
		printf("  new sp %x ra %x pc %x err %d z %x\n",
			ksc.sc_regs[SP], ksc.sc_regs[RA], ksc.sc_regs[PC],
			error, ksc.sc_regs[ZERO]);
#endif
		return (EINVAL);
	}
	scp = &ksc;
	/*
	 * Restore the user supplied information
	 */
	if (scp->sc_onstack & 01)
		p->p_sigacts->ps_sigstk.ss_flags |= SA_ONSTACK;
	else
		p->p_sigacts->ps_sigstk.ss_flags &= ~SA_ONSTACK;
	p->p_sigmask = scp->sc_mask &~ sigcantmask;
	regs[PC] = scp->sc_pc;
	bcopy((caddr_t)&scp->sc_regs[1], (caddr_t)&regs[1],
		sizeof(scp->sc_regs) - sizeof(int));
	if (scp->sc_fpused)
		bcopy((caddr_t)scp->sc_fpregs, (caddr_t)&p->p_md.md_regs[F0],
			sizeof(scp->sc_fpregs));
	return (EJUSTRETURN);
}

int	waittime = -1;

boot(howto)
	register int howto;
{

	/* take a snap shot before clobbering any registers */
	if (curproc)
		savectx(curproc->p_addr, 0);

	howto |= RB_HALT; /* XXX */
	boothowto = howto;
	if ((howto&RB_NOSYNC) == 0 && waittime < 0) {
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
		void (*f)() = (void (*)())MACH_MON_REINIT;

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

/*
 * Convert an ASCII string into an integer.
 */
int
atoi(s)
	char *s;
{
	int c;
	unsigned base = 10, d;
	int neg = 0, val = 0;

	if (s == 0 || (c = *s++) == 0)
		goto out;

	/* skip spaces if any */
	while (c == ' ' || c == '\t')
		c = *s++;

	/* parse sign, allow more than one (compat) */
	while (c == '-') {
		neg = !neg;
		c = *s++;
	}

	/* parse base specification, if any */
	if (c == '0') {
		c = *s++;
		switch (c) {
		case 'X':
		case 'x':
			base = 16;
			break;
		case 'B':
		case 'b':
			base = 2;
			break;
		default:
			base = 8;
			break;
		}
	}

	/* parse number proper */
	for (;;) {
		if (c >= '0' && c <= '9')
			d = c - '0';
		else if (c >= 'a' && c <= 'z')
			d = c - 'a' + 10;
		else if (c >= 'A' && c <= 'Z')
			d = c - 'A' + 10;
		else
			break;
		val *= base;
		val += d;
		c = *s++;
	}
	if (neg)
		val = -val;
out:
	return val;	
}

#ifdef DS5000
/* 
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 * 
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS 
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 * 
 * Carnegie Mellon requests users of this software to return to
 * 
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 * 
 * any improvements or extensions that they make and grant Carnegie the
 * rights to redistribute these changes.
 */

#include <pmax/pmax/turbochannel.h>

/*
 * Driver map: associates a device driver to an option type.
 * Drivers name are (arbitrarily) defined in each driver and
 * used in the various config tables.
 */
struct drivers_map {
	char	module_name[TC_ROM_LLEN];	/* from ROM, literally! */
	char	*driver_name;			/* in bus_??_init[] tables */
} tc_drivers_map[] = {
	{ "KN02    ",	"dc"},		/* system board, serial I/O */
	{ "PMAD-AA ",	"le"},		/* Ether */
	{ "PMAZ-AA ",	"asc"},		/* SCSI */
	{ "PMAG-BA ",	"cfb"},		/* Color Frame Buffer */
	{ "PMAG-CA ",	"ga"},		/* 2D graphic board */
	{ "PMAG-DA ",	"gq"},		/* 3D graphic board (LM) */
	{ "PMAG-FA ",	"gq"},		/* 3D graphic board (HE) */

	{ "", 0}			/* list end */
};

#ifdef DEBUG
int tc_verbose = 0;
#endif

/*
 * TURBOchannel autoconf procedure. Finds in one sweep what is
 * hanging on the bus and fills in the tc_slot_info array.
 * This is only the first part of the autoconf scheme, at this
 * time we are basically only looking for a graphics board and
 * serial port to use as system console (all workstations).
 *
 * XXX Someday make number of slots dynamic too.
 */

#define KN02_TC_NSLOTS	8

tc_option_t	tc_slot_info[KN02_TC_NSLOTS];

caddr_t	tc_slot_virt_addr[] = {
	(caddr_t)0xbe000000,	/* TURBOchannel, slot 0 */
	(caddr_t)0xbe400000,	/* TURBOchannel, slot 1 */
	(caddr_t)0xbe800000,	/* TURBOchannel, slot 2 */
	(caddr_t)0xbec00000,	/* TURBOchannel, slot 3 */
	(caddr_t)0xbf000000,	/* TURBOchannel, slot 4 */
	(caddr_t)0xbf400000,	/* TURBOchannel, slot 5 */
	(caddr_t)0xbf800000,	/* TURBOchannel, slot 6 */
/*	(caddr_t)0xbfc00000,	   TURBOchannel, slot 7 */
};

void
tc_find_all_options()
{
	register int i;
	caddr_t addr;
	register tc_option_t *sl;
	register struct drivers_map *map;
	register struct pmax_ctlr *cp;
	register struct driver *drp;

	/*
	 * Look for all controllers on the bus.
	 */
	i = sizeof(tc_slot_virt_addr) / sizeof(tc_slot_virt_addr[0]) - 1;
	while (i >= 0) {
		addr = tc_slot_virt_addr[i];
		if (tc_probe_slot(addr, &tc_slot_info[i])) {
			/*
			 * Found a slot, make a note of it 
			 */
			tc_slot_info[i].present = 1;
			tc_slot_info[i].module_address = addr;
		}

		i -= tc_slot_info[i].slot_size;
	}

	/*
	 * Now for each slot found, see if we have a device driver that
	 * handles it.
	 */
	for (i = 0, sl = tc_slot_info; i < KN02_TC_NSLOTS; i++, sl++) {
		if (!sl->present)
			continue;
		/*
		 * Look for mapping between the module name and
		 * the device driver name.
		 */
		for (map = tc_drivers_map; map->driver_name; map++) {
			if (bcmp(sl->module_name, map->module_name, TC_ROM_LLEN))
				continue;
			goto fnd_map;
		}
#ifdef DEBUG
		if (tc_verbose)
			printf("Cannot associate a device driver to %s\n",
				sl->module_name);
#endif
		sl->present = 0;
		continue;

		/*
		 * Find the device driver entry and fill in the address.
		 */
	fnd_map:
		for (cp = pmax_cinit; drp = cp->pmax_driver; cp++) {
			if (strcmp(drp->d_name, map->driver_name))
				continue;
			if (cp->pmax_addr == (char *)QUES) {
				cp->pmax_addr = sl->module_address;
				cp->pmax_pri = i;
				continue;
			}
			if (cp->pmax_addr != sl->module_address) {
				cp->pmax_addr = (char *)QUES;
				printf("%s: device not at configued address (expected at %x, found at %x)\n",
					drp->d_name,
					cp->pmax_addr, sl->module_address);
			}
		}
	}
}

/*
 * Probe a slot in the TURBOchannel. Return TRUE if a valid option
 * is present, FALSE otherwise. A side-effect is to fill the slot
 * descriptor with the size of the option, whether it is
 * recognized or not.
 */
int
tc_probe_slot(addr, slot)
	caddr_t addr;
	tc_option_t *slot;
{
	int i;
	static unsigned tc_offset_rom[] = {
		TC_OFF_PROTO_ROM, TC_OFF_ROM
	};
#define TC_N_OFFSETS	sizeof(tc_offset_rom)/sizeof(unsigned)

	slot->slot_size = 1;

	for (i = 0; i < TC_N_OFFSETS; i++) {
		if (badaddr(addr + tc_offset_rom[i], 4))
			continue;
		/* complain only on last chance */
		if (tc_identify_option((tc_rommap_t *)(addr + tc_offset_rom[i]),
		    slot, i == (TC_N_OFFSETS-1)))
			return (1);
	}
	return (0);
#undef TC_N_OFFSETS
}

/*
 * Identify an option on the TURBOchannel.  Looks at the mandatory
 * info in the option's ROM and checks it.
 */
int
tc_identify_option(addr, slot, complain)
	tc_rommap_t *addr;
	tc_option_t *slot;
	int complain;
{
	register int i;
	unsigned char width;
	char firmwr[TC_ROM_LLEN+1];
	char vendor[TC_ROM_LLEN+1];
	char module[TC_ROM_LLEN+1];
	char host_type[TC_ROM_SLEN+1];

	/*
	 * We do not really use the 'width' info, but take advantage
	 * of the restriction that the spec impose on the portion
	 * of the ROM that maps between +0x3e0 and +0x470, which
	 * is the only piece we need to look at.
	 */
	width = addr->rom_width.value;
	switch (width) {
	case 1:
	case 2:
	case 4:
		break;

	default:
		if (complain)
			printf("Invalid ROM width (0x%x) at x%x\n",
			       width, addr);
		return (0);
	}

	if (addr->rom_stride.value != 4) {
		if (complain)
			printf("Invalid ROM stride (0x%x) at x%x\n",
			       addr->rom_stride.value, addr);
		return (0);
	}

	if (addr->test_data[0] != 0x55 ||
	    addr->test_data[4] != 0x00 ||
	    addr->test_data[8] != 0xaa ||
	    addr->test_data[12] != 0xff) {
		if (complain)
			printf("Test pattern failed, option at x%x\n",
			       addr);
		return (0);
	}

	for (i = 0; i < TC_ROM_LLEN; i++) {
		firmwr[i] = addr->firmware_rev[i].value;
		vendor[i] = addr->vendor_name[i].value;
		module[i] = addr->module_name[i].value;
		if (i >= TC_ROM_SLEN)
			continue;
		host_type[i] = addr->host_firmware_type[i].value;
	}

#ifdef DEBUG
	if (tc_verbose) {
		firmwr[TC_ROM_LLEN] = '\0';
		vendor[TC_ROM_LLEN] = '\0';
		module[TC_ROM_LLEN] = '\0';
		host_type[TC_ROM_SLEN] = '\0';
		printf("%s %s '%s' at x%x\n %s %s %s '%s'\n %s %d %s %d %s\n",
		       "Found a", vendor, module, addr,
		       "Firmware rev.", firmwr,
		       "diagnostics for a", host_type,
		       "ROM size is", addr->rom_size.value << 3,
		       "Kbytes, uses", addr->slot_size.value, "TC slot(s)");
	}
#endif

	bcopy(module, slot->module_name, TC_ROM_LLEN);
	bcopy(vendor, slot->module_id, TC_ROM_LLEN);
	bcopy(firmwr, &slot->module_id[TC_ROM_LLEN], TC_ROM_LLEN);
	slot->slot_size = addr->slot_size.value;
	slot->rom_width = width;

	return (1);
}

/*
 * Enable/Disable interrupts for a TURBOchannel slot.
 */
tc_enable_interrupt(slotno, on)
	register int slotno;
	int on;
{
	register volatile int *p_csr = (volatile int *)MACH_SYS_CSR_ADDR;
	int csr;
	int s;

	slotno = 1 << (slotno + MACH_CSR_IOINTEN_SHIFT);
	s = Mach_spl0();
	csr = *p_csr & ~(MACH_CSR_MBZ | 0xFF);
	if (on)
		*p_csr = csr | slotno;
	else
		*p_csr = csr & ~slotno;
	splx(s);
}

#endif /* DS5000 */
