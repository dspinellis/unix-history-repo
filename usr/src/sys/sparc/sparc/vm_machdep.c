/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vm_machdep.c	7.4 (Berkeley) %G%
 *
 * from: $Header: vm_machdep.c,v 1.10 92/11/26 03:05:11 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/malloc.h>
#include <sys/buf.h>
#include <sys/exec.h>
#include <sys/vnode.h>

#include <vm/vm.h>
#include <vm/vm_kern.h>

#include <machine/cpu.h>
#include <machine/frame.h>

/*
 * Move pages from one kernel virtual address to another.
 */
pagemove(from, to, size)
	register caddr_t from, to;
	int size;
{
	register vm_offset_t pa;

	if (size & CLOFSET || (int)from & CLOFSET || (int)to & CLOFSET)
		panic("pagemove 1");
	while (size > 0) {
		pa = pmap_extract(kernel_pmap, (vm_offset_t)from);
		if (pa == 0)
			panic("pagemove 2");
		pmap_remove(kernel_pmap,
		    (vm_offset_t)from, (vm_offset_t)from + PAGE_SIZE);
		pmap_enter(kernel_pmap,
		    (vm_offset_t)to, pa, VM_PROT_READ|VM_PROT_WRITE, 1);
		from += PAGE_SIZE;
		to += PAGE_SIZE;
		size -= PAGE_SIZE;
	}
}

/*
 * Map an IO request into kernel virtual address space.
 *
 * ###	pmap_enter distributes this mapping to all contexts ... maybe
 *	we should avoid this extra work
 *
 * THIS IS NOT IDEAL -- WE NEED ONLY VIRTUAL SPACE BUT kmem_alloc_wait
 * DOES WORK DESIGNED TO SUPPLY PHYSICAL SPACE ON DEMAND LATER
 */
vmapbuf(bp)
	register struct buf *bp;
{
	register int npf;
	register caddr_t addr;
	struct proc *p;
	int off;
	vm_offset_t kva;
	register vm_offset_t pa;

	if ((bp->b_flags & B_PHYS) == 0)
		panic("vmapbuf");
	addr = bp->b_saveaddr = bp->b_un.b_addr;
	off = (int)addr & PGOFSET;
	p = bp->b_proc;
	npf = btoc(round_page(bp->b_bcount + off));
	kva = kmem_alloc_wait(phys_map, ctob(npf));
	bp->b_un.b_addr = (caddr_t) (kva + off);
	while (npf--) {
		pa = pmap_extract(vm_map_pmap(&p->p_vmspace->vm_map),
		    (vm_offset_t)addr);
		if (pa == 0)
			panic("vmapbuf: null page frame");
		pmap_enter(vm_map_pmap(phys_map), kva,
		    trunc_page(pa) | PMAP_NC,
		    VM_PROT_READ|VM_PROT_WRITE, 1);
		addr += PAGE_SIZE;
		kva += PAGE_SIZE;
	}
}

/*
 * Free the io map addresses associated with this IO operation.
 */
vunmapbuf(bp)
	register struct buf *bp;
{
	register vm_offset_t kva = (vm_offset_t)bp->b_un.b_addr;
	register int off, npf;

	if ((bp->b_flags & B_PHYS) == 0)
		panic("vunmapbuf");
	off = (int)kva & PGOFSET;
	kva -= off;
	npf = btoc(round_page(bp->b_bcount + off));
	kmem_free_wakeup(phys_map, kva, ctob(npf));
	bp->b_un.b_addr = bp->b_saveaddr;
	bp->b_saveaddr = NULL;
	cache_flush(bp->b_un.b_addr, bp->b_bcount - bp->b_resid);
}

/*
 * Allocate physical memory space in the dvma virtual address range.
 */
caddr_t
dvma_malloc(size)
	size_t size;
{
	vm_size_t vsize;
	caddr_t va;

	vsize = round_page(size);
	va = (caddr_t)kmem_alloc(phys_map, vsize);
	if (va == NULL)
		panic("dvma_malloc");
	kvm_uncache(va, vsize >> PGSHIFT);
	return (va);
}

/*
 * The offset of the topmost frame in the kernel stack.
 */
#define	TOPFRAMEOFF (UPAGES*NBPG-sizeof(struct trapframe)-sizeof(struct frame))

/*
 * Finish a fork operation, with process p2 nearly set up.
 * Copy and update the kernel stack and pcb, making the child
 * ready to run, and marking it so that it can return differently
 * than the parent.  Returns 1 in the child process, 0 in the parent.
 *
 * This function relies on the fact that the pcb is
 * the first element in struct user.
 */
cpu_fork(p1, p2)
	register struct proc *p1, *p2;
{
	register struct pcb *opcb = &p1->p_addr->u_pcb;
	register struct pcb *npcb = &p2->p_addr->u_pcb;
	register u_int sp, topframe, off, ssize;

	/*
	 * Save all the registers to p1's stack or, in the case of
	 * user registers and invalid stack pointers, to opcb.
	 * snapshot() also sets the given pcb's pcb_sp and pcb_psr
	 * to the current %sp and %psr, and sets pcb_pc to a stub
	 * which returns 1.  We then copy the whole pcb to p2;
	 * when swtch() selects p2 to run, it will run at the stub,
	 * rather than at the copying code below, and cpu_fork
	 * will return 1.
	 *
	 * Note that the order `*npcb = *opcb, snapshot(npcb)' is wrong,
	 * as user registers might then wind up only in opcb.
	 * We could call save_user_windows first,
	 * but that would only save 3 stores anyway.
	 *
	 * If process p1 has an FPU state, we must copy it.  If it is
	 * the FPU user, we must save the FPU state first.
	 */
	snapshot(opcb);
	bcopy((caddr_t)opcb, (caddr_t)npcb, sizeof(struct pcb));
	if (p1->p_md.md_fpstate) {
		if (p1 == fpproc)
			savefpstate(p1->p_md.md_fpstate);
		p2->p_md.md_fpstate = malloc(sizeof(struct fpstate),
		    M_SUBPROC, M_WAITOK);
		bcopy(p1->p_md.md_fpstate, p2->p_md.md_fpstate,
		    sizeof(struct fpstate));
	} else
		p2->p_md.md_fpstate = NULL;

	/*
	 * Copy the active part of the kernel stack,
	 * then adjust each kernel sp -- the frame pointer
	 * in the top frame is a user sp -- in the child's copy,
	 * including the initial one in the child's pcb.
	 */
	sp = npcb->pcb_sp;		/* points to old kernel stack */
	ssize = (u_int)opcb + UPAGES * NBPG - sp;
	if (ssize >= UPAGES * NBPG - sizeof(struct pcb))
		panic("cpu_fork 1");
	off = (u_int)npcb - (u_int)opcb;
	qcopy((caddr_t)sp, (caddr_t)sp + off, ssize);
	sp += off;
	npcb->pcb_sp = sp;
	topframe = (u_int)npcb + TOPFRAMEOFF;
	while (sp < topframe)
		sp = ((struct rwindow *)sp)->rw_in[6] += off;
	if (sp != topframe)
		panic("cpu_fork 2");
	/*
	 * This might be unnecessary, but it may be possible for the child
	 * to run in ptrace or sendsig before it returns from fork.
	 */
	p2->p_md.md_tf = (struct trapframe *)((int)p1->p_md.md_tf + off);
	return (0);
}

/*
 * cpu_exit is called as the last action during exit.
 * We release the address space and machine-dependent resources,
 * including the memory for the user structure and kernel stack.
 * Since the latter is also the interrupt stack, we release it
 * from assembly code after switching to a temporary pcb+stack.
 */
cpu_exit(p)
	struct proc *p;
{
	register struct fpstate *fs;

	if ((fs = p->p_md.md_fpstate) != NULL) {
		if (p == fpproc) {
			savefpstate(fs);
			fpproc = NULL;
		}
		free((void *)fs, M_SUBPROC);
	}
	vmspace_free(p->p_vmspace);
	swtchexit(kernel_map, p->p_addr, round_page(ctob(UPAGES)));
	/* NOTREACHED */
}

/*
 * cpu_coredump is called to write a core dump header.
 * (should this be defined elsewhere?  machdep.c?)
 */
int
cpu_coredump(p, vp, cred)
	struct proc *p;
	struct vnode *vp;
	struct ucred *cred;
{
	register struct user *up = p->p_addr;

	up->u_md.md_tf = *p->p_md.md_tf;
	if (p->p_md.md_fpstate)
		up->u_md.md_fpstate = *p->p_md.md_fpstate;
	else
		bzero((caddr_t)&up->u_md.md_fpstate, sizeof(struct fpstate));
	return (vn_rdwr(UIO_WRITE, vp, (caddr_t)up, ctob(UPAGES), (off_t)0,
	    UIO_SYSSPACE, IO_NODELOCKED|IO_UNIT, cred, (int *)NULL, p));
}
