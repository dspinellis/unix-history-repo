/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)genassym.c	7.7 (Berkeley) %G%
 */

#define KERNEL

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/map.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <machine/reg.h>
#include <machine/pte.h>

main()
{
	register struct proc *p = (struct proc *)0;
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

	printf("#define\tU_PCB_SEGTAB %d\n", &up->u_pcb.pcb_segtab);
	printf("#define\tVM_MIN_ADDRESS 0x%x\n", VM_MIN_ADDRESS);
	printf("#define\tVM_MIN_KERNEL_ADDRESS 0x%x\n", VM_MIN_KERNEL_ADDRESS);

	printf("#define\tNBPW %d\n", NBPW);

	printf("#define\tSIGILL %d\n", SIGILL);
	printf("#define\tSIGFPE %d\n", SIGFPE);

	exit(0);
}
