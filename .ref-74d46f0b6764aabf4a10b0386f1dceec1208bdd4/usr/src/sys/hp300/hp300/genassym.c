/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)genassym.c	7.5 (Berkeley) %G%
 */

#define KERNEL

#include "sys/param.h"
#include "sys/buf.h"
#include "sys/vmmeter.h"
#include "sys/user.h"
#include "sys/cmap.h"
#include "sys/map.h"
#include "sys/proc.h"
#include "sys/mbuf.h"
#include "sys/msgbuf.h"
#include "../include/cpu.h"
#include "../include/trap.h"
#include "../include/psl.h"
#include "../include/reg.h"
#include "clockreg.h"
#include "sys/syscall.h"
#include "vm/vm_param.h"
#include "vm/vm_map.h"
#include "../include/pmap.h"

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

	printf("#define\tP_LINK %d\n", &p->p_link);
	printf("#define\tP_RLINK %d\n", &p->p_rlink);
	printf("#define\tP_MAP %d\n", &p->p_map);
	printf("#define\tPMAP %d\n", &map->pmap);
	printf("#define\tPM_STCHG %d\n", &pmap->pm_stchanged);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_PRI %d\n", &p->p_pri);
	printf("#define\tP_STAT %d\n", &p->p_stat);
	printf("#define\tP_WCHAN %d\n", &p->p_wchan);
	printf("#define\tP_FLAG %d\n", &p->p_flag);
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
	printf("#define\tNKMEMCLUSTERS %d\n", NKMEMCLUSTERS);
	printf("#define\tU_PROCP %d\n", &up->u_procp);
	printf("#define\tU_RU %d\n", &up->u_ru);
	printf("#define\tU_PROF %d\n", &up->u_prof);
	printf("#define\tU_PROFSCALE %d\n", &up->u_prof.pr_scale);
	printf("#define\tRU_MINFLT %d\n", &rup->ru_minflt);
	printf("#define\tT_BUSERR %d\n", T_BUSERR);
	printf("#define\tT_ADDRERR %d\n", T_ADDRERR);
	printf("#define\tT_ILLINST %d\n", T_ILLINST);
	printf("#define\tT_ZERODIV %d\n", T_ZERODIV);
	printf("#define\tT_CHKINST %d\n", T_CHKINST);
	printf("#define\tT_TRAPVINST %d\n", T_TRAPVINST);
	printf("#define\tT_PRIVINST %d\n", T_PRIVINST);
	printf("#define\tT_TRACE %d\n", T_TRACE);
	printf("#define\tT_MMUFLT %d\n", T_MMUFLT);
	printf("#define\tT_SSIR %d\n", T_SSIR);
	printf("#define\tT_FMTERR %d\n", T_FMTERR);
	printf("#define\tT_COPERR %d\n", T_COPERR);
	printf("#define\tT_FPERR %d\n", T_FPERR);
	printf("#define\tT_ASTFLT %d\n", T_ASTFLT);
	printf("#define\tT_TRAP15 %d\n", T_TRAP15);
	printf("#define\tPSL_S %d\n", PSL_S);
	printf("#define\tPSL_IPL7 %d\n", PSL_IPL7);
	printf("#define\tPSL_LOWIPL %d\n", PSL_LOWIPL);
	printf("#define\tPSL_HIGHIPL %d\n", PSL_HIGHIPL);
	printf("#define\tPSL_USER %d\n", PSL_USER);
	printf("#define\tSPL1 %d\n", PSL_S | PSL_IPL1);
	printf("#define\tSPL2 %d\n", PSL_S | PSL_IPL2);
	printf("#define\tSPL3 %d\n", PSL_S | PSL_IPL3);
	printf("#define\tSPL4 %d\n", PSL_S | PSL_IPL4);
	printf("#define\tSPL5 %d\n", PSL_S | PSL_IPL5);
	printf("#define\tSPL6 %d\n", PSL_S | PSL_IPL6);
	printf("#define\tFC_USERD %d\n", FC_USERD);
	printf("#define\tFC_PURGE %d\n", FC_PURGE);
	printf("#define\tMAXADDR %d\n", MAXADDR);
	printf("#define\tIOMAPSIZE %d\n", IOMAPSIZE);
	printf("#define\tIOBASE %d\n", IOBASE);
	printf("#define\tMMUBASE %d\n", MMUBASE);
	printf("#define\tMMUSTAT %d\n", MMUSTAT);
	printf("#define\tMMUCMD %d\n", MMUCMD);
	printf("#define\tMMUSSTP %d\n", MMUSSTP);
	printf("#define\tMMUUSTP %d\n", MMUUSTP);
	printf("#define\tMMUTBINVAL %d\n", MMUTBINVAL);
	printf("#define\tMMU_BERR %d\n", MMU_BERR);
	printf("#define\tMMU_ENAB %d\n", MMU_ENAB);
	printf("#define\tMMU_FAULT %d\n", MMU_FAULT);
	printf("#define\tMMU_CEN %d\n", MMU_CEN);
	printf("#define\tMMU_IEN %d\n", MMU_IEN);
	printf("#define\tMMU_FPE %d\n", MMU_FPE);
	printf("#define\tCACHE_ON %d\n", CACHE_ON);
	printf("#define\tCACHE_OFF %d\n", CACHE_OFF);
	printf("#define\tCACHE_CLR %d\n", CACHE_CLR);
	printf("#define\tIC_CLEAR %d\n", IC_CLEAR);
	printf("#define\tDC_CLEAR %d\n", DC_CLEAR);
	printf("#define\tPG_V %d\n", PG_V);
	printf("#define\tPG_NV %d\n", PG_NV);
	printf("#define\tPG_RO %d\n", PG_RO);
	printf("#define\tPG_RW %d\n", PG_RW);
	printf("#define\tPG_CI %d\n", PG_CI);
	printf("#define\tPG_PROT %d\n", PG_PROT);
	printf("#define\tPG_FRAME %d\n", PG_FRAME);
	printf("#define\tSG_V %d\n", SG_V);
	printf("#define\tSG_NV %d\n", SG_NV);
	printf("#define\tSG_RW %d\n", SG_RW);
	printf("#define\tSG_FRAME %d\n", SG_FRAME);
	printf("#define\tSG_ISHIFT %d\n", SG_ISHIFT);
	printf("#define\tPCB_FLAGS %d\n", &pcb->pcb_flags);
	printf("#define\tPCB_PS %d\n", &pcb->pcb_ps);
	printf("#define\tPCB_USTP %d\n", &pcb->pcb_ustp);
	printf("#define\tPCB_USP %d\n", &pcb->pcb_usp);
	printf("#define\tPCB_REGS %d\n", pcb->pcb_regs);
	printf("#define\tPCB_CMAP2 %d\n", &pcb->pcb_cmap2);
	printf("#define\tPCB_SSWAP %d\n", &pcb->pcb_sswap);
	printf("#define\tPCB_SIGC %d\n", pcb->pcb_sigc);
	printf("#define\tPCB_ONFAULT %d\n", &pcb->pcb_onfault);
	printf("#define\tPCB_FPCTX %d\n", &pcb->pcb_fpregs);
	printf("#define\tSP %d\n", SP);
	printf("#define\tB_READ %d\n", B_READ);
	printf("#define\tENOENT %d\n", ENOENT);
	printf("#define\tEFAULT %d\n", EFAULT);
	printf("#define\tENAMETOOLONG %d\n", ENAMETOOLONG);
	printf("#define\tCLKBASE %d\n", CLKBASE);
	printf("#define\tCLKCR1 %d\n", CLKCR1);
	printf("#define\tCLKCR2 %d\n", CLKCR2);
	printf("#define\tCLKCR3 %d\n", CLKCR3);
	printf("#define\tCLKSR %d\n", CLKSR);
	printf("#define\tCLKMSB1 %d\n", CLKMSB1);
	printf("#define\tCLKMSB2 %d\n", CLKMSB2);
	printf("#define\tCLKMSB3 %d\n", CLKMSB3);
	printf("#define\tSYS_exit %d\n", SYS_exit);
	printf("#define\tSYS_execv %d\n", SYS_execv);
	printf("#define\tSYS_sigreturn %d\n", SYS_sigreturn);
	for (i = 0; i < 32; i++) {
		if ((1 << i) & SPTECHG)
			printf("#define\tSPTECHGB %d\n", i);
		if ((1 << i) & PCB_AST)
			printf("#define\tPCB_ASTB %d\n", i);
		if ((1 << i) & PCB_HPUXTRACE)
			printf("#define\tPCB_TRCB %d\n", i);
	}
	exit(0);
}
