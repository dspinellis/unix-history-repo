/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)genassym.c	7.8 (Berkeley) 9/23/93";
#endif /* not lint */

#include "sys/param.h"
#include "sys/vmmeter.h"
#include "sys/vmparam.h"
#include "sys/buf.h"
#include "sys/user.h"
#include "sys/cmap.h"
#include "sys/map.h"
#include "sys/proc.h"
#include "sys/text.h"
#include "sys/mbuf.h"
#include "sys/msgbuf.h"

#include "../include/pte.h"
#include "../tahoe/scb.h"

#include "../vba/vbaparam.h"

main()
{
	register struct user *u = (struct user *)0;
	register struct proc *p = (struct proc *)0;
	register struct vmmeter *vm = (struct vmmeter *)0;
	register struct pcb *pcb = (struct pcb *)0;
	register struct scb *scb = (struct scb *)0;

	printf("#ifdef LOCORE\n");
	printf("#define\tU_PROCP %d\n", &u->u_procp);
	printf("#define\tP_FORW %d\n", &p->p_forw);
	printf("#define\tP_BACK %d\n", &p->p_back);
	printf("#define\tP_XLINK %d\n", &p->p_xlink);
	printf("#define\tP_ADDR %d\n", &p->p_addr);
	printf("#define\tP_PRIORITY %d\n", &p->p_priority);
	printf("#define\tP_STAT %d\n", &p->p_stat);
	printf("#define\tP_WCHAN %d\n", &p->p_wchan);
	printf("#define\tP_TSIZE %d\n", &p->p_tsize);
	printf("#define\tP_SSIZE %d\n", &p->p_ssize);
	printf("#define\tP_P0BR %d\n", &p->p_p0br);
	printf("#define\tP_SZPT %d\n", &p->p_szpt);
	printf("#define\tP_TEXTP %d\n", &p->p_textp);
	printf("#define\tP_FLAG %d\n", &p->p_flag);
	printf("#define\tP_DKEY %d\n", &p->p_dkey);
	printf("#define\tP_CKEY %d\n", &p->p_ckey);
	printf("#define\tSSLEEP %d\n", SSLEEP);
	printf("#define\tSRUN %d\n", SRUN);
	printf("#define\tV_SWTCH %d\n", &vm->v_swtch);
	printf("#define\tV_TRAP %d\n", &vm->v_trap);
	printf("#define\tV_SYSCALL %d\n", &vm->v_syscall);
	printf("#define\tV_INTR %d\n", &vm->v_intr);
	printf("#define\tV_SOFT %d\n", &vm->v_soft);
	printf("#define\tV_FPE %d\n", &vm->v_fpe);
	printf("#define\tV_ALIGN %d\n", &vm->v_align);
	printf("#define\tMCLBYTES %d\n", MCLBYTES);
	printf("#define\tNBPG %d\n", NBPG);
	printf("#define\tPGSHIFT %d\n", PGSHIFT);
	printf("#define\tUPAGES %d\n", UPAGES);
	printf("#define\tCLSIZE %d\n", CLSIZE);
	printf("#define\tMAXPHYS %d\n", MAXPHYS);
	printf("#define\tSYSPTSIZE %d\n", SYSPTSIZE);
	printf("#define\tUSRPTSIZE %d\n", USRPTSIZE);
	printf("#define\tVBIOSIZE %d\n", VBIOSIZE);
	printf("#define\tMSGBUFPTECNT %d\n", btoc(sizeof (struct msgbuf)));
	printf("#define\tNMBCLUSTERS %d\n", NMBCLUSTERS);
	printf("#define\tNKMEMCLUSTERS %d\n", NKMEMCLUSTERS);
#ifdef SYSVSHM
	printf("#define\tSHMMAXPGS %d\n", SHMMAXPGS);
#endif
	printf("#define\tPCB_KSP %d\n", &pcb->pcb_ksp);
	printf("#define\tPCB_USP %d\n", &pcb->pcb_usp);
	printf("#define\tPCB_R0 %d\n", &pcb->pcb_r0);
	printf("#define\tPCB_R1 %d\n", &pcb->pcb_r1);
	printf("#define\tPCB_R2 %d\n", &pcb->pcb_r2);
	printf("#define\tPCB_R3 %d\n", &pcb->pcb_r3);
	printf("#define\tPCB_R4 %d\n", &pcb->pcb_r4);
	printf("#define\tPCB_R5 %d\n", &pcb->pcb_r5);
	printf("#define\tPCB_R6 %d\n", &pcb->pcb_r6);
	printf("#define\tPCB_R7 %d\n", &pcb->pcb_r7);
	printf("#define\tPCB_R8 %d\n", &pcb->pcb_r8);
	printf("#define\tPCB_R9 %d\n", &pcb->pcb_r9);
	printf("#define\tPCB_R10 %d\n", &pcb->pcb_r10);
	printf("#define\tPCB_R11 %d\n", &pcb->pcb_r11);
	printf("#define\tPCB_R12 %d\n", &pcb->pcb_r12);
	printf("#define\tPCB_R13 %d\n", &pcb->pcb_r13);
	printf("#define\tPCB_FP %d\n", &pcb->pcb_fp);
	printf("#define\tPCB_PC %d\n", &pcb->pcb_pc);
	printf("#define\tPCB_PSL %d\n", &pcb->pcb_psl);
	printf("#define\tPCB_P0BR %d\n", &pcb->pcb_p0br);
	printf("#define\tPCB_P0LR %d\n", &pcb->pcb_p0lr);
	printf("#define\tPCB_P1BR %d\n", &pcb->pcb_p1br);
	printf("#define\tPCB_P1LR %d\n", &pcb->pcb_p1lr);
	printf("#define\tPCB_P2BR %d\n", &pcb->pcb_p2br);
	printf("#define\tPCB_P2LR %d\n", &pcb->pcb_p2lr);
	printf("#define\tPCB_ACH %d\n", &pcb->pcb_ach);
	printf("#define\tPCB_ACL %d\n", &pcb->pcb_acl);
	printf("#define\tPCB_HFS %d\n", &pcb->pcb_hfs);
	printf("#define\tPCB_SAVACC %d\n", &pcb->pcb_savacc);
	printf("#define\tPCB_SZPT %d\n", &pcb->pcb_szpt);
	printf("#define\tPCB_CMAP2 %d\n", &pcb->pcb_cmap2);
	printf("#define\tPCB_SSWAP %d\n", &pcb->pcb_sswap);
	printf("#define\tPCB_SIGC %d\n", pcb->pcb_sigc);
	printf("#define\tSCB_DOADUMP %d\n", &scb->scb_doadump);
	printf("#define\tSCB_BUSERR %d\n", &scb->scb_buserr);
#define	SCB_DEVBASE	(((int)((struct scb *)0)->scb_devint)/sizeof (int))
	printf("#define\tSCB_DEVBASE %d\n", SCB_DEVBASE);
	printf("#endif\n");
	exit(0);
}
