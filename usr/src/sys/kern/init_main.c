/*
 * Copyright (c) 1982, 1986, 1989, 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)init_main.c	7.55 (Berkeley) %G%
 */

#include "param.h"
#include "filedesc.h"
#include "kernel.h"
#include "mount.h"
#include "map.h"
#include "proc.h"
#include "resourcevar.h"
#include "signalvar.h"
#include "systm.h"
#include "vnode.h"
#include "conf.h"
#include "buf.h"
#include "clist.h"
#include "protosw.h"
#include "reboot.h"
#include "user.h"


#include "machine/cpu.h"

#include "vm/vm.h"

#ifdef HPFPLIB
char	copyright[] =
"Copyright (c) 1982,1986,1989,1991 The Regents of the University of California.\nCopyright (c) 1992 Hewlett-Packard Company\nCopyright (c) 1992 Motorola Inc.\nAll rights reserved.\n\n";
#else
char	copyright[] =
"Copyright (c) 1982,1986,1989,1991 The Regents of the University of California.\nAll rights reserved.\n\n";
#endif

/*
 * Components of process 0;
 * never freed.
 */
struct	session session0;
struct	pgrp pgrp0;
struct	proc proc0;
struct	pcred cred0;
struct	filedesc0 filedesc0;
struct	plimit limit0;
struct	vmspace vmspace0;
struct	proc *curproc = &proc0;
struct	proc *initproc, *pageproc;

int	cmask = CMASK;
extern	struct user *proc0paddr;
extern	int (*mountroot)();

struct	vnode *rootvp, *swapdev_vp;
int	boothowto;
struct	timeval boottime;
struct	timeval runtime;

/*
 * System startup; initialize the world, create process 0,
 * mount root filesystem, and fork to create init and pagedaemon.
 * Most of the hard work is done in the lower-level initialization
 * routines including startup(), which does memory initialization
 * and autoconfiguration.
 */
main()
{
	register int i;
	register struct proc *p;
	register struct filedesc0 *fdp;
	int s, rval[2];
	extern void roundrobin __P((void *));
	extern void schedcpu __P((void *));

	/*
	 * Initialize curproc before any possible traps/probes
	 * to simplify trap processing.
	 */
	p = &proc0;
	curproc = p;
	/*
	 * Attempt to find console and initialize
	 * in case of early panic or other messages.
	 */
	consinit();
	printf(copyright);

	vm_mem_init();
	kmeminit();
	cpu_startup();

	/*
	 * set up system process 0 (swapper)
	 */
	p = &proc0;
	curproc = p;

	allproc = (volatile struct proc *)p;
	p->p_prev = (struct proc **)&allproc;
	p->p_pgrp = &pgrp0;
	pgrphash[0] = &pgrp0;
	pgrp0.pg_mem = p;
	pgrp0.pg_session = &session0;
	session0.s_count = 1;
	session0.s_leader = p;

	p->p_flag = SLOAD|SSYS;
	p->p_stat = SRUN;
	p->p_nice = NZERO;
	bcopy("swapper", p->p_comm, sizeof ("swapper"));

	/*
	 * Setup credentials
	 */
	cred0.p_refcnt = 1;
	p->p_cred = &cred0;
	p->p_ucred = crget();
	p->p_ucred->cr_ngroups = 1;	/* group 0 */

	/*
	 * Create the file descriptor table for process 0.
	 */
	fdp = &filedesc0;
	p->p_fd = &fdp->fd_fd;
	fdp->fd_fd.fd_refcnt = 1;
	fdp->fd_fd.fd_cmask = cmask;
	fdp->fd_fd.fd_ofiles = fdp->fd_dfiles;
	fdp->fd_fd.fd_ofileflags = fdp->fd_dfileflags;
	fdp->fd_fd.fd_nfiles = NDFILE;

	/*
	 * Set initial limits
	 */
	p->p_limit = &limit0;
	for (i = 0; i < sizeof(p->p_rlimit)/sizeof(p->p_rlimit[0]); i++)
		limit0.pl_rlimit[i].rlim_cur =
		    limit0.pl_rlimit[i].rlim_max = RLIM_INFINITY;
	limit0.pl_rlimit[RLIMIT_OFILE].rlim_cur = NOFILE;
	limit0.pl_rlimit[RLIMIT_NPROC].rlim_cur = MAXUPRC;
	limit0.p_refcnt = 1;

	/*
	 * Allocate a prototype map so we have something to fork
	 */
	p->p_vmspace = &vmspace0;
	vmspace0.vm_refcnt = 1;
	pmap_pinit(&vmspace0.vm_pmap);
	vm_map_init(&p->p_vmspace->vm_map, round_page(VM_MIN_ADDRESS),
	    trunc_page(VM_MAX_ADDRESS), TRUE);
	vmspace0.vm_map.pmap = &vmspace0.vm_pmap;
	p->p_addr = proc0paddr;				/* XXX */

	/*
	 * We continue to place resource usage info
	 * and signal actions in the user struct so they're pageable.
	 */
	p->p_stats = &p->p_addr->u_stats;
	p->p_sigacts = &p->p_addr->u_sigacts;

	/*
	 * Initialize per uid information structure and charge
	 * root for one process.
	 */
	usrinfoinit();
	(void)chgproccnt(0, 1);

	rqinit();

	/*
	 * configure virtual memory system,
	 * set vm rlimits
	 */
	vm_init_limits(p);

	/*
	 * Initialize the file systems.
	 */
	vfsinit();

	/*
	 * Start real time and statistics clocks.
	 */
	initclocks();

	/*
	 * Initialize tables, protocols, and set up well-known inodes.
	 */
	mbinit();
	cinit();
#ifdef SYSVSHM
	shminit();
#endif
#include "sl.h"
#if NSL > 0
	slattach();			/* XXX */
#endif
#include "loop.h"
#if NLOOP > 0
	loattach();			/* XXX */
#endif
	/*
	 * Block reception of incoming packets
	 * until protocols have been initialized.
	 */
	s = splimp();
	ifinit();
	domaininit();
	splx(s);

#ifdef GPROF
	kmstartup();
#endif

	/* kick off timeout driven events by calling first time */
	roundrobin(NULL);
	schedcpu(NULL);

	/*
	 * Set up the root file system and vnode.
	 */
	if ((*mountroot)())
		panic("cannot mount root");
	/*
	 * Get vnode for '/'.
	 * Setup rootdir and fdp->fd_fd.fd_cdir to point to it.
	 */
	if (VFS_ROOT(rootfs, &rootdir))
		panic("cannot find root vnode");
	fdp->fd_fd.fd_cdir = rootdir;
	VREF(fdp->fd_fd.fd_cdir);
	VOP_UNLOCK(rootdir);
	fdp->fd_fd.fd_rdir = NULL;
	swapinit();

	/*
	 * Now can look at time, having had a chance to verify the time
	 * from the file system.  Reset p->p_rtime as it may have been
	 * munched in swtch() after the time got set.
	 */
	p->p_stats->p_start = runtime = mono_time = boottime = time;
	p->p_rtime.tv_sec = p->p_rtime.tv_usec = 0;

	/*
	 * make init process
	 */
	siginit(p);
	if (fork(p, NULL, rval))
		panic("fork init");
	if (rval[1]) {
		extern int icode[];		/* user init code */
		extern int szicode;		/* size of icode */
		static char initflags[] = "-sf";
		vm_offset_t addr;
		char *ip;

		/*
		 * Now in process 1.  Set init flags into icode, get a minimal
		 * address space, copy out "icode", and return to it to do an
		 * exec of init.
		 */
		ip = initflags + 1;
		if (boothowto&RB_SINGLE)
			*ip++ = 's';
#ifdef notyet
		if (boothowto&RB_FASTBOOT)
			*ip++ = 'f';
#endif
		if (ip == initflags + 1)
			*ip++ = '-';
		*ip++ = '\0';

		addr = VM_MIN_ADDRESS;
		initproc = p = curproc;
		if (vm_allocate(&p->p_vmspace->vm_map, &addr,
		    round_page(szicode + sizeof(initflags)), FALSE) != 0 ||
		    addr != VM_MIN_ADDRESS)
			panic("init: couldn't allocate at zero");

		/* need just enough stack to exec from */
		addr = trunc_page(USRSTACK - PAGE_SIZE);
		if (vm_allocate(&p->p_vmspace->vm_map, &addr,
		    PAGE_SIZE, FALSE) != KERN_SUCCESS)
			panic("vm_allocate init stack");
		p->p_vmspace->vm_maxsaddr = (caddr_t)addr;
		(void)copyout((caddr_t)icode, (caddr_t)VM_MIN_ADDRESS,
		    (u_int)szicode);
		(void)copyout(initflags, (caddr_t)(VM_MIN_ADDRESS + szicode),
		    sizeof(initflags));
		return;			/* returns to icode */
	}

	/*
	 * Start up pageout daemon (process 2).
	 */
	if (fork(p, NULL, rval))
		panic("fork pager");
	if (rval[1]) {
		/*
		 * Now in process 2.
		 */
		p = curproc;
		pageproc = p;
		p->p_flag |= SLOAD|SSYS;		/* XXX */
		bcopy("pagedaemon", curproc->p_comm, sizeof ("pagedaemon"));
		vm_pageout();
		/*NOTREACHED*/
	}

	/*
	 * enter scheduling loop
	 */
	sched();
}
