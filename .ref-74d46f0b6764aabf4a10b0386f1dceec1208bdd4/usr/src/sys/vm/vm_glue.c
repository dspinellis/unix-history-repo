/* 
 * Copyright (c) 1987 Carnegie-Mellon University
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * The Mach Operating System project at Carnegie-Mellon University.
 *
 * The CMU software License Agreement specifies the terms and conditions
 * for use and redistribution.
 *
 *	@(#)vm_glue.c	7.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "buf.h"

#include "../vm/vm_param.h"
#include "../vm/vm_map.h"
#include "../vm/vm_page.h"
#include "../vm/vm_kern.h"

int	avefree = 0;		/* XXX */
unsigned maxdmap = MAXDSIZ;	/* XXX */

kernacc(addr, len, rw)
	caddr_t addr;
	int len, rw;
{
	boolean_t rv;
	vm_prot_t prot = rw == B_READ ? VM_PROT_READ : VM_PROT_WRITE;

	rv = vm_map_check_protection(kernel_map, trunc_page(addr),
				     round_page(addr+len-1), prot);
	return(rv == TRUE);
}

useracc(addr, len, rw)
	caddr_t addr;
	int len, rw;
{
	boolean_t rv;
	vm_prot_t prot = rw == B_READ ? VM_PROT_READ : VM_PROT_WRITE;

	rv = vm_map_check_protection(u.u_procp->p_map, trunc_page(addr),
				     round_page(addr+len-1), prot);
	return(rv == TRUE);
}

#ifdef KGDB
/*
 * Change protections on kernel pages from addr to addr+size
 * (presumably so debugger can plant a breakpoint).
 * All addresses are assumed to reside in the Sysmap,
 */
chgkprot(addr, len, rw)
	register caddr_t addr;
	int len, rw;
{
	vm_prot_t prot = rw == B_READ ? VM_PROT_READ : VM_PROT_WRITE;

	vm_map_protect(kernel_map, trunc_page(addr),
		       round_page(addr+len-1), prot, FALSE);
}
#endif

vslock(addr, len)
	caddr_t	addr;
	u_int	len;
{
	vm_map_pageable(u.u_procp->p_map, trunc_page(addr),
			round_page(addr+len-1), FALSE);
}

vsunlock(addr, len, dirtied)
	caddr_t	addr;
	u_int	len;
	int dirtied;
{
#ifdef	lint
	dirtied++;
#endif	lint
	vm_map_pageable(u.u_procp->p_map, trunc_page(addr),
			round_page(addr+len-1), TRUE);
}

procdup(p, isvfork)
	register struct proc *p;
	int isvfork;
{
	register struct user *up;
	vm_offset_t addr;
	vm_size_t size;

#if 0
	/*
	 * Duplicate the process address space.
	 * XXX if this is a vfork we arrange to share data/stack to
	 *     preserve brain-dead semantics of vfork().
	 * XXX this doesn't work due to a bug in the VM code.
	 *     Once a process has done a vfork setting up sharing maps,
	 *     any future forks may fail as the source VM range doesn't
	 *     properly get write-protected.  This causes the parent to
	 *     not create copies and instead modifies the originals.
	 *     If the parent activates before the child, the child will
	 *     get a corrupted address space.
	 */
	if (isvfork) {
		addr = trunc_page(u.u_daddr);
		size = ctob(u.u_dsize);
		(void) vm_map_inherit(u.u_procp->p_map, addr,
				      addr + size, VM_INHERIT_SHARE);
		(void) vm_map_inherit(u.u_procp->p_map, u.u_maxsaddr,
				      VM_MAX_ADDRESS, VM_INHERIT_SHARE);
	}
#endif
	p->p_map = vm_map_fork(u.u_procp->p_map);
#if 0
	if (isvfork) {
		(void) vm_map_inherit(u.u_procp->p_map, addr,
				      addr + size, VM_INHERIT_COPY);
		(void) vm_map_inherit(u.u_procp->p_map, u.u_maxsaddr,
				      VM_MAX_ADDRESS, VM_INHERIT_COPY);
	}
#endif
	/*
	 * Allocate a wired-down (for now) u-area for the process
	 */
	size = round_page(ctob(UPAGES));
	addr = kmem_alloc_pageable(kernel_map, size);
	vm_map_pageable(kernel_map, addr, addr+size, FALSE);
	p->p_addr = (caddr_t)addr;
	up = (struct user *)addr;

	/*
	 * Update the current u-area and copy it to the new one
	 */
	resume(pcbb(u.u_procp));
	bcopy(u.u_procp->p_addr, p->p_addr, size);
	up->u_procp = p;
	PMAP_ACTIVATE(p->p_map->pmap, (struct pcb *)p->p_addr);

	/*
	 * Arrange for a non-local goto when the new process
	 * is started, to resume here, returning nonzero from setjmp.
	 */
	up->u_pcb.pcb_sswap = (int *)&u.u_ssave;
	if (savectx(&up->u_ssave)) {
		/*
		 * Return 1 in child.
		 */
		return (1);
	}

	/*
	 * Clear vm statistics of new process.
	 */
	bzero((caddr_t)&up->u_ru, sizeof (struct rusage));
	bzero((caddr_t)&up->u_cru, sizeof (struct rusage));
	up->u_outime = 0;
	return (0);
}

/*
 * XXX Scaled down version from vm_page.c
 */
vminit()
{
	/*
	 * Set up the initial limits on process VM.
	 * Set the maximum resident set size to be all
	 * of (reasonably) available memory.  This causes
	 * any single, large process to start random page
	 * replacement once it fills memory.
	 */
        u.u_rlimit[RLIMIT_STACK].rlim_cur = DFLSSIZ;
        u.u_rlimit[RLIMIT_STACK].rlim_max = MAXSSIZ;
        u.u_rlimit[RLIMIT_DATA].rlim_cur = DFLDSIZ;
        u.u_rlimit[RLIMIT_DATA].rlim_max = MAXDSIZ;
	u.u_rlimit[RLIMIT_RSS].rlim_cur = u.u_rlimit[RLIMIT_RSS].rlim_max =
		ptoa(vm_page_free_count);
	proc[0].p_maxrss = vm_page_free_count;
}

#include "../vm/vm_pageout.h"

#ifdef DEBUG
int	enableswap = 1;
int	swapdebug = 0;
#define	SDB_FOLLOW	1
#define SDB_SWAPIN	2
#define SDB_SWAPOUT	4
#endif

/*
 * Brutally simple:
 *	1. Attempt to swapin every swaped-out, runnable process in
 *	   order of priority.
 *	2. If not enough memory, wake the pageout daemon and let it
 *	   clear some space.
 */
sched()
{
	register struct proc *rp;
	register int rppri;
	struct proc *inp;
	int inpri;
	vm_offset_t addr;
	vm_size_t size;

loop:
#ifdef DEBUG
	if (!enableswap) {
		inp = NULL;
		goto noswap;
	}
#endif
	wantin = 0;
	inp = NULL;
	inpri = -20000;
	for (rp = allproc; rp != NULL; rp = rp->p_nxt)
		if (rp->p_stat == SRUN && (rp->p_flag & SLOAD) == 0) {
			rppri = rp->p_time +
				rp->p_slptime - (rp->p_nice-NZERO)*8;
			if (rppri > inpri) {
				inp = rp;
				inpri = rppri;
			}
		}
#ifdef DEBUG
	if (swapdebug & SDB_FOLLOW)
		printf("sched: running, procp %x pri %d\n", inp, inpri);
noswap:
#endif
	/*
	 * Nothing to do, back to sleep
	 */
	if ((rp = inp) == NULL) {
		(void) splhigh();
		runout++;
		sleep((caddr_t)&runout, PVM);
		(void) spl0();
		goto loop;
	}
	/*
	 * We would like to bring someone in.
	 * This part is really bogus cuz we could deadlock on memory
	 * despite our feeble check.
	 */
	size = round_page(ctob(UPAGES));
	addr = (vm_offset_t) rp->p_addr;
	if (vm_page_free_count > atop(size)) {
#ifdef DEBUG
		if (swapdebug & SDB_SWAPIN)
			printf("swapin: pid %d(%s)@%x, pri %d free %d\n",
			       rp->p_pid, rp->p_comm, rp->p_addr,
			       inpri, vm_page_free_count);
#endif
		vm_map_pageable(kernel_map, addr, addr+size, FALSE);
		(void) splclock();
		if (rp->p_stat == SRUN)
			setrq(rp);
		rp->p_flag |= SLOAD;
		(void) spl0();
		rp->p_time = 0;
		goto loop;
	}
	/*
	 * Not enough memory, jab the pageout daemon and wait til the
	 * coast is clear.
	 */
#ifdef DEBUG
	if (swapdebug & SDB_FOLLOW)
		printf("sched: no room for pid %d(%s), free %d\n",
		       rp->p_pid, rp->p_comm, vm_page_free_count);
#endif
	(void) splhigh();
	VM_WAIT;
	(void) spl0();
#ifdef DEBUG
	if (swapdebug & SDB_FOLLOW)
		printf("sched: room again, free %d\n", vm_page_free_count);
#endif
	goto loop;
}

#define	swappable(p) \
	(((p)->p_flag & (SSYS|SULOCK|SLOAD|SKEEP|SWEXIT|SPHYSIO)) == SLOAD)

/*
 * Swapout is driven by the pageout daemon.  Very simple, we find eligible
 * procs and unwire their u-areas.  We try to always "swap" at least one
 * process in case we need the room for a swapin.
 */
swapout_threads()
{
	register struct proc *rp;
	struct proc *outp, *outp2;
	int outpri, outpri2;
	int didswap = 0;
	extern int maxslp;

#ifdef DEBUG
	if (!enableswap)
		return;
#endif
	outp = outp2 = NULL;
	outpri = outpri2 = -20000;
	for (rp = allproc; rp != NULL; rp = rp->p_nxt) {
		if (!swappable(rp))
			continue;
		switch(rp->p_stat) {
		case SRUN:
			if (rp->p_slptime > outpri2) {
				outp2 = rp;
				outpri2 = rp->p_slptime;
			}
			continue;
			
		case SSLEEP:
		case SSTOP:
			if (rp->p_slptime > maxslp) {
				swapout(rp);
				didswap++;
			} else if (rp->p_slptime > outpri) {
				outp = rp;
				outpri = rp->p_slptime;
			}
			continue;
		}
	}
	/*
	 * If we didn't get rid of any real duds, toss out the next most
	 * likely sleeping/stopped or running candidate.  We only do this
	 * if we are real low on memory since we don't gain much by doing
	 * it (UPAGES pages).
	 */
	if (didswap == 0 &&
	    vm_page_free_count <= atop(round_page(ctob(UPAGES)))) {
		if ((rp = outp) == 0)
			rp = outp2;
#ifdef DEBUG
		if (swapdebug & SDB_SWAPOUT)
			printf("swapout_threads: no duds, try procp %x\n", rp);
#endif
		if (rp)
			swapout(rp);
	}
}

swapout(p)
	register struct proc *p;
{
	vm_offset_t addr;
	vm_size_t size;

#ifdef DEBUG
	if (swapdebug & SDB_SWAPOUT)
		printf("swapout: pid %d(%s)@%x, stat %x pri %d free %d\n",
		       p->p_pid, p->p_comm, p->p_addr, p->p_stat,
		       p->p_slptime, vm_page_free_count);
#endif
	size = round_page(ctob(UPAGES));
	addr = (vm_offset_t) p->p_addr;
	vm_map_pageable(kernel_map, addr, addr+size, TRUE);
	pmap_collect(vm_map_pmap(p->p_map));
	(void) splhigh();
	p->p_flag &= ~SLOAD;
	if (p->p_stat == SRUN)
		remrq(p);
	(void) spl0();
	p->p_time = 0;
}

/*
 * The rest of these routines fake thread handling
 */

void
assert_wait(event, ruptible)
	int event;
	boolean_t ruptible;
{
#ifdef lint
	ruptible++;
#endif
	u.u_procp->p_thread = event;
}

void
thread_block()
{
	int s = splhigh();

	if (u.u_procp->p_thread)
		sleep((caddr_t)u.u_procp->p_thread, PVM);
	splx(s);
}

void
thread_sleep(event, lock, ruptible)
	int event;
	simple_lock_t lock;
	boolean_t ruptible;
{
#ifdef lint
	ruptible++;
#endif
	int s = splhigh();

	u.u_procp->p_thread = event;
	simple_unlock(lock);
	if (u.u_procp->p_thread)
		sleep((caddr_t)u.u_procp->p_thread, PVM);
	splx(s);
}

void
thread_wakeup(event)
	int event;
{
	int s = splhigh();

	wakeup((caddr_t)event);
	splx(s);
}

/*
 * DEBUG stuff
 */

int indent = 0;

/*ARGSUSED2*/
iprintf(a, b, c, d, e, f, g, h)
	char *a;
{
	register int i;

	for (i = indent; i > 0; ) {
		if (i >= 8) {
			putchar('\t', 1, (caddr_t)0);
			i -= 8;
		} else {
			putchar(' ', 1, (caddr_t)0);
			i--;
		}
	}
	printf(a, b, c, d, e, f, g, h);
}
