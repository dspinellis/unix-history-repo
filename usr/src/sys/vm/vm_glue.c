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
 *	@(#)vm_glue.c	7.2 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "proc.h"
#include "resourcevar.h"
#include "buf.h"
#include "user.h"

#include "vm.h"
#include "vm_page.h"
#include "vm_kern.h"

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

	rv = vm_map_check_protection(&curproc->p_vmspace->vm_map,
	    trunc_page(addr), round_page(addr+len-1), prot);
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
	vm_map_pageable(&curproc->p_vmspace->vm_map, trunc_page(addr),
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
	vm_map_pageable(&curproc->p_vmspace->vm_map, trunc_page(addr),
			round_page(addr+len-1), TRUE);
}

vm_fork(p1, p2, isvfork)
	register struct proc *p1, *p2;
	int isvfork;
{
	register struct user *up;
	vm_offset_t addr;
	vm_size_t size;

	p2->p_vmspace = vmspace_fork(p1->p_vmspace);

#ifdef SYSVSHM
	if (p1->p_vmspace->vm_shm)
		shmfork(p1, p2, isvfork);
#endif

	/*
	 * Allocate a wired-down (for now) u-area for the process
	 */
	size = round_page(ctob(UPAGES));
	addr = kmem_alloc_pageable(kernel_map, size);
	vm_map_pageable(kernel_map, addr, addr + size, FALSE);
	p2->p_addr = (caddr_t)addr;
	up = (struct user *)addr;

	/*
	 * Update the current u-area and copy it to the new one
	 * THIS SHOULD BE DONE DIFFERENTLY, probably with a single
	 * machine-dependent call that copies and updates the pcb+stack,
	 * replacing the resume and savectx.
	 */
	resume(pcbb(p1));
	bcopy(p1->p_addr, p2->p_addr, size);
	/*
	 * p_stats and p_sigacts currently point at fields
	 * in the user struct but not at &u, instead at p_addr.
	 */
	p2->p_stats = &((struct user *)p2->p_addr)->u_stats;
	p2->p_sigacts = &((struct user *)p2->p_addr)->u_sigacts;

	/*
	 * Clear vm statistics of new process.
	 */
	bzero((caddr_t)&up->u_stats.p_ru, sizeof (struct rusage));
	bzero((caddr_t)&up->u_stats.p_cru, sizeof (struct rusage));

	PMAP_ACTIVATE(&p2->p_vmspace->vm_pmap, (struct pcb *)p2->p_addr, 0);

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
	return (0);
}

/*
 * Set default limits for VM system.
 * Called for proc 0, and then inherited by all others.
 */
vm_init_limits(p)
	register struct proc *p;
{

	/*
	 * Set up the initial limits on process VM.
	 * Set the maximum resident set size to be all
	 * of (reasonably) available memory.  This causes
	 * any single, large process to start random page
	 * replacement once it fills memory.
	 */
        p->p_rlimit[RLIMIT_STACK].rlim_cur = DFLSSIZ;
        p->p_rlimit[RLIMIT_STACK].rlim_max = MAXSSIZ;
        p->p_rlimit[RLIMIT_DATA].rlim_cur = DFLDSIZ;
        p->p_rlimit[RLIMIT_DATA].rlim_max = MAXDSIZ;
	p->p_rlimit[RLIMIT_RSS].rlim_cur = p->p_rlimit[RLIMIT_RSS].rlim_max =
		ptoa(vm_page_free_count);
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
	register struct proc *p;
	register int pri;
	struct proc *pp;
	int ppri;
	vm_offset_t addr;
	vm_size_t size;

loop:
#ifdef DEBUG
	if (!enableswap) {
		pp = NULL;
		goto noswap;
	}
#endif
	pp = NULL;
	ppri = INT_MIN;
	for (p = allproc; p != NULL; p = p->p_nxt)
		if (p->p_stat == SRUN && (p->p_flag & SLOAD) == 0) {
			pri = p->p_time + p->p_slptime - p->p_nice * 8;
			if (pri > ppri) {
				pp = p;
				ppri = pri;
			}
		}
#ifdef DEBUG
	if (swapdebug & SDB_FOLLOW)
		printf("sched: running, procp %x pri %d\n", pp, ppri);
noswap:
#endif
	/*
	 * Nothing to do, back to sleep
	 */
	if ((p = pp) == NULL) {
		sleep((caddr_t)&proc0, PVM);
		goto loop;
	}

	/*
	 * We would like to bring someone in.
	 * This part is really bogus cuz we could deadlock on memory
	 * despite our feeble check.
	 */
	size = round_page(ctob(UPAGES));
	addr = (vm_offset_t) p->p_addr;
	if (vm_page_free_count > atop(size)) {
#ifdef DEBUG
		if (swapdebug & SDB_SWAPIN)
			printf("swapin: pid %d(%s)@%x, pri %d free %d\n",
			       p->p_pid, p->p_comm, p->p_addr,
			       ppri, vm_page_free_count);
#endif
		vm_map_pageable(kernel_map, addr, addr+size, FALSE);
		(void) splclock();
		if (p->p_stat == SRUN)
			setrq(p);
		p->p_flag |= SLOAD;
		(void) spl0();
		p->p_time = 0;
		goto loop;
	}
	/*
	 * Not enough memory, jab the pageout daemon and wait til the
	 * coast is clear.
	 */
#ifdef DEBUG
	if (swapdebug & SDB_FOLLOW)
		printf("sched: no room for pid %d(%s), free %d\n",
		       p->p_pid, p->p_comm, vm_page_free_count);
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
	(((p)->p_flag & (SSYS|SLOAD|SKEEP|SWEXIT|SPHYSIO)) == SLOAD)

/*
 * Swapout is driven by the pageout daemon.  Very simple, we find eligible
 * procs and unwire their u-areas.  We try to always "swap" at least one
 * process in case we need the room for a swapin.
 * If any procs have been sleeping/stopped for at least maxslp seconds,
 * they are swapped.  Else, we swap the longest-sleeping or stopped process,
 * if any, otherwise the longest-resident process.
 */
swapout_threads()
{
	register struct proc *p;
	struct proc *outp, *outp2;
	int outpri, outpri2;
	int didswap = 0;
	extern int maxslp;

#ifdef DEBUG
	if (!enableswap)
		return;
#endif
	outp = outp2 = NULL;
	outpri = outpri2 = 0;
	for (p = allproc; p != NULL; p = p->p_nxt) {
		if (!swappable(p))
			continue;
		switch (p->p_stat) {
		case SRUN:
			if (p->p_time > outpri2) {
				outp2 = p;
				outpri2 = p->p_time;
			}
			continue;
			
		case SSLEEP:
		case SSTOP:
			if (p->p_slptime > maxslp) {
				swapout(p);
				didswap++;
			} else if (p->p_slptime > outpri) {
				outp = p;
				outpri = p->p_slptime;
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
		if ((p = outp) == 0)
			p = outp2;
#ifdef DEBUG
		if (swapdebug & SDB_SWAPOUT)
			printf("swapout_threads: no duds, try procp %x\n", p);
#endif
		if (p)
			swapout(p);
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
	pmap_collect(vm_map_pmap(&p->p_vmspace->vm_map));
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
	curproc->p_thread = event;
}

void
thread_block()
{
	int s = splhigh();

	if (curproc->p_thread)
		sleep((caddr_t)curproc->p_thread, PVM);
	splx(s);
}

thread_sleep(event, lock, ruptible)
	int event;
	simple_lock_t lock;
	boolean_t ruptible;
{
#ifdef lint
	ruptible++;
#endif
	int s = splhigh();

	curproc->p_thread = event;
	simple_unlock(lock);
	if (curproc->p_thread)
		sleep((caddr_t)event, PVM);
	splx(s);
}

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
