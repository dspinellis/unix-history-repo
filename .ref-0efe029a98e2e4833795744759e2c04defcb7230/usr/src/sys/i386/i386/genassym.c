/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)genassym.c	5.4 (Berkeley) %G%
 */

#include "../i386/pte.h"

#include "param.h"
#include "vmmeter.h"
#include "vmparam.h"
#include "buf.h"
#include "dir.h"
#include "user.h"
#include "cmap.h"
#include "map.h"
#include "proc.h"
#include "text.h"
#include "mbuf.h"
#include "msgbuf.h"

main()
{
	register struct user *u = (struct user *)0;
	register struct proc *p = (struct proc *)0;
	register struct vmmeter *vm = (struct vmmeter *)0;
	register struct pcb *pcb = (struct pcb *)0;

	printf("#ifdef LOCORE\n");
	printf("#define\tI386_CR3PAT %d\n", I386_CR3PAT);

	printf("#define\tU_PROCP %d\n", &u->u_procp);
	printf("#define\tU_EOSYS %d\n", &u->u_eosys);
	printf("#define\tP_LINK %d\n", &p->p_link);
	printf("#define\tP_RLINK %d\n", &p->p_rlink);
	printf("#define\tP_XLINK %d\n", &p->p_xlink);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_PRI %d\n", &p->p_pri);
	printf("#define\tP_STAT %d\n", &p->p_stat);
	printf("#define\tP_WCHAN %d\n", &p->p_wchan);
	printf("#define\tP_TSIZE %d\n", &p->p_tsize);
	printf("#define\tP_SSIZE %d\n", &p->p_ssize);
	printf("#define\tP_P0BR %d\n", &p->p_p0br);
	printf("#define\tP_SZPT %d\n", &p->p_szpt);
	printf("#define\tP_TEXTP %d\n", &p->p_textp);
	printf("#define\tP_FLAG %d\n", &p->p_flag);
	printf("#define\tP_CR3 %d\n", &p->p_cr3);
	printf("#define\tSSLEEP %d\n", SSLEEP);
	printf("#define\tSRUN %d\n", SRUN);
	printf("#define\tV_SWTCH %d\n", &vm->v_swtch);
	printf("#define\tV_TRAP %d\n", &vm->v_trap);
	printf("#define\tV_SYSCALL %d\n", &vm->v_syscall);
	printf("#define\tV_INTR %d\n", &vm->v_intr);
	printf("#define\tV_SOFT %d\n", &vm->v_soft);
	printf("#define\tP1PAGES %d\n", P1PAGES);
	printf("#define\tNBPG %d\n", NBPG);
	printf("#define\tPGSHIFT %d\n", PGSHIFT);
	printf("#define\tUPAGES %d\n", UPAGES);
	printf("#define\tCLSIZE %d\n", CLSIZE);
	printf("#define\tMAXPHYS %d\n", MAXPHYS);
	printf("#define\tSYSPTSIZE %d\n", SYSPTSIZE);
	printf("#define\tUSRPTSIZE %d\n", USRPTSIZE);
	printf("#define\tUSRIOSIZE %d\n", USRIOSIZE);
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
	printf("#define\tPCB_FPSAV %d\n", &pcb->pcb_fpsav);
	printf("#define\tPCB_SAVEFPU %d\n", &pcb->pcb_savefpu);
	printf("#define\tPCB_SAVEEMC %d\n", &pcb->pcb_saveemc);
	printf("#define\tPCB_P0BR %d\n", &pcb->pcb_p0br);
	printf("#define\tPCB_P1BR %d\n", &pcb->pcb_p1br);
	printf("#define\tPCB_P0LR %d\n", &pcb->pcb_p0lr);
	printf("#define\tPCB_P1LR %d\n", &pcb->pcb_p1lr);
	printf("#define\tPCB_SZPT %d\n", &pcb->pcb_szpt);
	printf("#define\tPCB_CMAP2 %d\n", &pcb->pcb_cmap2);
	printf("#define\tPCB_SSWAP %d\n", &pcb->pcb_sswap);
	printf("#define\tPCB_SIGC %d\n", pcb->pcb_sigc);
	printf("#define\tPCB_IML %d\n", &pcb->pcb_iml);

	printf("#endif\n");
	exit(0);
}
