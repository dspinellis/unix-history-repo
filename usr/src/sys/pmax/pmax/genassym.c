/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)genassym.c	7.3 (Berkeley) %G%
 */

#define KERNEL

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/map.h>
#include <sys/proc.h>
#include <sys/mbuf.h>
#include <sys/user.h>

#include <machine/reg.h>
#include <machine/pmap.h>

main()
{
	register struct proc *p = (struct proc *)0;
	register struct user *up = (struct user *)0;
	register pmap_hash_t hp = (pmap_hash_t)PMAP_HASH_UADDR;
	register struct vmmeter *vm = (struct vmmeter *)0;
	register int size, s, n;

	printf("#define\tP_LINK %d\n", &p->p_link);
	printf("#define\tP_RLINK %d\n", &p->p_rlink);
	printf("#define\tP_PRI %d\n", &p->p_pri);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_UPTE %d\n", p->p_md.md_upte);
	printf("#define\tU_PCB_REGS %d\n", up->u_pcb.pcb_regs);
	printf("#define\tU_PCB_FPREGS %d\n", &up->u_pcb.pcb_regs[F0]);
	printf("#define\tU_PCB_CONTEXT %d\n", &up->u_pcb.pcb_context);
	printf("#define\tU_PCB_ONFAULT %d\n", &up->u_pcb.pcb_onfault);
	printf("#define\tPMAP_HASH_LOW_OFFSET 0x%x\n", &hp->pmh_pte[0].low);
	printf("#define\tPMAP_HASH_HIGH_OFFSET 0x%x\n", &hp->pmh_pte[0].high);
	printf("#define\tPMAP_HASH_KPAGES %d\n", PMAP_HASH_KPAGES);
	printf("#define\tPMAP_HASH_KADDR 0x%x\n", PMAP_HASH_KADDR);
	printf("#define\tPMAP_HASH_SIZE_SHIFT %d\n", PMAP_HASH_SIZE_SHIFT);
	printf("#define\tPMAP_HASH_SHIFT1 %d\n", PMAP_HASH_SHIFT1);
	printf("#define\tPMAP_HASH_SHIFT2 %d\n", PMAP_HASH_SHIFT2);
	printf("#define\tPMAP_HASH_MASK1 0x%x\n", PMAP_HASH_MASK1);
	printf("#define\tPMAP_HASH_MASK2 0x%x\n", PMAP_HASH_MASK2);
	printf("#define\tV_SWTCH %d\n", &vm->v_swtch);
	printf("#define\tSIGILL %d\n", SIGILL);
	printf("#define\tSIGFPE %d\n", SIGFPE);
	exit(0);
}
