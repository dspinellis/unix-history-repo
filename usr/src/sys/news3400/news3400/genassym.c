/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)genassym.c	7.3 (Berkeley) %G%
 */

#define KERNEL

#include <machine/fix_machine_type.h>

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/dir.h>
#include <sys/map.h>
#include <sys/proc.h>
#include <sys/mbuf.h>
#include <sys/user.h>
#include <sys/msgbuf.h>
#include <sys/syscall.h>
#include <sys/signal.h>
#include <vm/vm.h>
#include <machine/reg.h>
#include <machine/pte.h>
#include <machine/pmap.h>

main()
{
	register struct proc *p = (struct proc *)0;
	register pmap_hash_t hp = (pmap_hash_t)PMAP_HASH_UADDR;
	register struct vmmeter *vm = (struct vmmeter *)0;
	register struct user *up = (struct user *)0;
	register struct rusage *rup = (struct rusage *)0;
	register struct pcb *pcb = (struct pcb *)0;

	/*
	 * struct proc
	 */
	printf("#define\tP_LINK %d\n", &p->p_link);
	printf("#define\tP_RLINK %d\n", &p->p_rlink);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_UPTE %d\n", p->p_md.md_upte);
	printf("#define\tP_PRI %d\n", &p->p_pri);
	printf("#define\tP_STAT %d\n", &p->p_stat);
	printf("#define\tP_PID %d\n", &p->p_pid);
	printf("#define\tP_WCHAN %d\n", &p->p_wchan);
	printf("#define\tP_FLAG %d\n", &p->p_flag);

	/*
	 * struct vmmeter
	 */
	printf("#define\tV_SWTCH %d\n", &vm->v_swtch);
	printf("#define\tV_TRAP %d\n", &vm->v_trap);
	printf("#define\tV_SYSCALL %d\n", &vm->v_syscall);
	printf("#define\tV_INTR %d\n", &vm->v_intr);
	printf("#define\tV_SOFT %d\n", &vm->v_soft);
	printf("#define\tV_FAULTS %d\n", &vm->v_faults);

	/*
	 * struct user
	 */
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

	printf("#define\tNBPW %d\n", NBPW);

	printf("#define\tSIGILL %d\n", SIGILL);
	printf("#define\tSIGFPE %d\n", SIGFPE);

	printf("#define\tSYS_execve %d\n", SYS_execve);
	printf("#define\tSYS_exit %d\n", SYS_exit);

	exit(0);
}
