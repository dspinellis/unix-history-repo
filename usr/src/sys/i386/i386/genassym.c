/*-
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)genassym.c	5.7 (Berkeley) %G%
 */

#ifndef lint
static char sccsid[] = "@(#)genassym.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include "sys/param.h"
#include "sys/buf.h"
#include "sys/vmmeter.h"
#include "sys/user.h"
#include "sys/cmap.h"
#include "sys/map.h"
#include "sys/proc.h"
#include "sys/mbuf.h"
#include "sys/msgbuf.h"
#include "machine/cpu.h"
#include "machine/trap.h"
#include "machine/psl.h"
#include "machine/reg.h"
#include "sys/syscall.h"
#include "vm/vm_param.h"
#include "vm/vm_map.h"
#include "machine/pmap.h"

main()
{
	register struct proc *p = (struct proc *)0;
	register struct vmmeter *vm = (struct vmmeter *)0;
	register struct user *up = (struct user *)0;
	register struct rusage *rup = (struct rusage *)0;
	vm_map_t map = (vm_map_t)0;
	pmap_t pmap = (pmap_t)0;
	struct pcb *pcb = (struct pcb *)0;
	register unsigned i;

	printf("#define\tI386_CR3PAT %d\n", I386_CR3PAT);
	printf("#define\tU_PROCP %d\n", &up->u_procp);
	printf("#define\tUDOT_SZ %d\n", sizeof(struct user));
	printf("#define\tP_LINK %d\n", &p->p_link);
	printf("#define\tP_RLINK %d\n", &p->p_rlink);
	printf("#define\tP_MAP %d\n", &p->p_map);
	printf("#define\tPMAP %d\n", &map->pmap);
	printf("#define\tPM_STCHG %d\n", &pmap->pm_pdchanged);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_PRI %d\n", &p->p_pri);
	printf("#define\tP_STAT %d\n", &p->p_stat);
	printf("#define\tP_WCHAN %d\n", &p->p_wchan);
	printf("#define\tP_FLAG %d\n", &p->p_flag);
	printf("#define\tP_PID %d\n", &p->p_pid);
	printf("#define\tSSLEEP %d\n", SSLEEP);
	printf("#define\tSRUN %d\n", SRUN);
	printf("#define\tV_SWTCH %d\n", &vm->v_swtch);
	printf("#define\tV_TRAP %d\n", &vm->v_trap);
	printf("#define\tV_SYSCALL %d\n", &vm->v_syscall);
	printf("#define\tV_INTR %d\n", &vm->v_intr);
	printf("#define\tV_SOFT %d\n", &vm->v_soft);
	printf("#define\tV_PDMA %d\n", &vm->v_pdma);
	printf("#define\tV_FAULTS %d\n", &vm->v_faults);
	printf("#define\tV_PGREC %d\n", &vm->v_pgrec);
	printf("#define\tV_FASTPGREC %d\n", &vm->v_fastpgrec);
	printf("#define\tUPAGES %d\n", UPAGES);
	printf("#define\tHIGHPAGES %d\n", HIGHPAGES);
	printf("#define\tP1PAGES %d\n", P1PAGES);
	printf("#define\tCLSIZE %d\n", CLSIZE);
	printf("#define\tNBPG %d\n", NBPG);
	printf("#define\tNPTEPG %d\n", NPTEPG);
	printf("#define\tPGSHIFT %d\n", PGSHIFT);
	printf("#define\tSYSPTSIZE %d\n", SYSPTSIZE);
	printf("#define\tUSRPTSIZE %d\n", USRPTSIZE);
	printf("#define\tUSRIOSIZE %d\n", USRIOSIZE);
#ifdef SYSVSHM
	printf("#define\tSHMMAXPGS %d\n", SHMMAXPGS);
#endif
	printf("#define\tUSRSTACK %d\n", USRSTACK);
	printf("#define\tMSGBUFPTECNT %d\n", btoc(sizeof (struct msgbuf)));
	printf("#define\tNMBCLUSTERS %d\n", NMBCLUSTERS);
	printf("#define\tMCLBYTES %d\n", MCLBYTES);
	printf("#define\tPCB_LINK %d\n", &pcb->pcbtss.tss_link);
	printf("#define\tPCB_ESP0 %d\n", &pcb->pcbtss.tss_esp0);
	printf("#define\tPCB_SS0 %d\n", &pcb->pcbtss.tss_ss0);
	printf("#define\tPCB_ESP1 %d\n", &pcb->pcbtss.tss_esp1);
	printf("#define\tPCB_SS1 %d\n", &pcb->pcbtss.tss_ss1);
	printf("#define\tPCB_ESP2 %d\n", &pcb->pcbtss.tss_esp2);
	printf("#define\tPCB_SS2 %d\n", &pcb->pcbtss.tss_ss2);
	printf("#define\tPCB_CR3 %d\n", &pcb->pcbtss.tss_cr3);
	printf("#define\tPCB_EIP %d\n", &pcb->pcbtss.tss_eip);
	printf("#define\tPCB_EFLAGS %d\n", &pcb->pcbtss.tss_eflags);
	printf("#define\tPCB_EAX %d\n", &pcb->pcbtss.tss_eax);
	printf("#define\tPCB_ECX %d\n", &pcb->pcbtss.tss_ecx);
	printf("#define\tPCB_EDX %d\n", &pcb->pcbtss.tss_edx);
	printf("#define\tPCB_EBX %d\n", &pcb->pcbtss.tss_ebx);
	printf("#define\tPCB_ESP %d\n", &pcb->pcbtss.tss_esp);
	printf("#define\tPCB_EBP %d\n", &pcb->pcbtss.tss_ebp);
	printf("#define\tPCB_ESI %d\n", &pcb->pcbtss.tss_esi);
	printf("#define\tPCB_EDI %d\n", &pcb->pcbtss.tss_edi);
	printf("#define\tPCB_ES %d\n", &pcb->pcbtss.tss_es);
	printf("#define\tPCB_CS %d\n", &pcb->pcbtss.tss_cs);
	printf("#define\tPCB_SS %d\n", &pcb->pcbtss.tss_ss);
	printf("#define\tPCB_DS %d\n", &pcb->pcbtss.tss_ds);
	printf("#define\tPCB_FS %d\n", &pcb->pcbtss.tss_fs);
	printf("#define\tPCB_GS %d\n", &pcb->pcbtss.tss_gs);
	printf("#define\tPCB_LDT %d\n", &pcb->pcbtss.tss_ldt);
	printf("#define\tPCB_IOOPT %d\n", &pcb->pcbtss.tss_ioopt);
	printf("#define\tNKMEMCLUSTERS %d\n", NKMEMCLUSTERS);
	printf("#define\tU_PROCP %d\n", &up->u_procp);
	printf("#define\tU_RU %d\n", &up->u_ru);
	printf("#define\tU_PROF %d\n", &up->u_prof);
	printf("#define\tU_PROFSCALE %d\n", &up->u_prof.pr_scale);
	printf("#define\tRU_MINFLT %d\n", &rup->ru_minflt);
	printf("#define\tPCB_FLAGS %d\n", &pcb->pcb_flags);
	printf("#define\tFP_WASUSED %d\n", FP_WASUSED);
	printf("#define\tFP_NEEDSSAVE %d\n", FP_NEEDSSAVE);
	printf("#define\tFP_NEEDSRESTORE %d\n", FP_NEEDSRESTORE);
	printf("#define\tFP_USESEMC %d\n", FP_USESEMC);
	printf("#define\tPCB_SAVEFPU %d\n", &pcb->pcb_savefpu);
	printf("#define\tPCB_SAVEEMC %d\n", &pcb->pcb_saveemc);
	printf("#define\tPCB_CMAP2 %d\n", &pcb->pcb_cmap2);
	printf("#define\tPCB_SSWAP %d\n", &pcb->pcb_sswap);
	printf("#define\tPCB_SIGC %d\n", pcb->pcb_sigc);
	printf("#define\tPCB_IML %d\n", &pcb->pcb_iml);

	printf("#define\tB_READ %d\n", B_READ);
	printf("#define\tENOENT %d\n", ENOENT);
	printf("#define\tEFAULT %d\n", EFAULT);
	printf("#define\tENAMETOOLONG %d\n", ENAMETOOLONG);
	exit(0);
}
