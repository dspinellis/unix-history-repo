/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *
 *	@(#)genassym.c	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: genassym.c,v 1.15 93/04/21 06:09:30 torek Exp $ (LBL)
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/map.h>
#include <sys/proc.h>
#include <sys/mbuf.h>
#include <sys/msgbuf.h>
#include <sys/syscall.h>
#include <sys/user.h>

#include <machine/pmap.h>
#include <machine/psl.h>

#ifdef notyet
#include <sparc/dev/zsreg.h>
#include <sparc/dev/zsvar.h>
#endif
#include <sparc/dev/bsd_audioreg.h>
#include <sparc/dev/bsd_audiovar.h>

#include <sparc/sparc/intreg.h>

#include <stdio.h>
#include <stddef.h>

#define	off(what, str, mem) def(what, (int)offsetof(str, mem))

void
def(what, where)
	char *what;
	int where;
{

	if (printf("#define\t%s\t%d\n", what, where) < 0) {
		perror("printf");
		exit(1);
	}
}

void
flush()
{

	if (fflush(stdout)) {
		perror("fflush");
		exit(1);
	}
}

main()
{

	/* general constants */
	def("BSD", BSD);
	def("UPAGES", UPAGES);
	def("NBPG", NBPG);
	def("KERNBASE", KERNBASE);
	def("USRSTACK", USRSTACK);

	/* proc fields and values */
	off("P_ADDR", struct proc, p_addr);
	off("P_STAT", struct proc, p_stat);
	off("P_WCHAN", struct proc, p_wchan);
	off("P_VMSPACE", struct proc, p_vmspace);
	off("P_RTIME", struct proc, p_rtime);
	def("SRUN", SRUN);

	/* VM structure fields */
	off("VM_PMAP", struct vmspace, vm_pmap);
	off("VM_PMAP_CTX", struct vmspace, vm_pmap.pm_ctx);
	off("VM_PMAP_CTXNUM", struct vmspace, vm_pmap.pm_ctxnum);

	/* interrupt/fault metering */
	off("V_SWTCH", struct vmmeter, v_swtch);
	off("V_INTR", struct vmmeter, v_intr);
	off("V_FAULTS", struct vmmeter, v_faults);

	/* PTE bits and related information */
	def("PG_W", PG_W);
	def("PG_VSHIFT", PG_VSHIFT);
	def("PG_PROTSHIFT", PG_PROTSHIFT);
	def("PG_PROTUREAD", PG_PROTUREAD);
	def("PG_PROTUWRITE", PG_PROTUWRITE);

	/* FPU state */
	off("FS_REGS", struct fpstate, fs_regs);
	off("FS_FSR", struct fpstate, fs_fsr);
	off("FS_QSIZE", struct fpstate, fs_qsize);
	off("FS_QUEUE", struct fpstate, fs_queue);
	def("FSR_QNE", FSR_QNE);

	/* system calls */
	def("SYS_sigreturn", SYS_sigreturn);
	def("SYS_execve", SYS_execve);
	def("SYS_exit", SYS_exit);

	/* errno */
	def("EFAULT", EFAULT);
	def("ENAMETOOLONG", ENAMETOOLONG);

	/* PCB fields */
	off("PCB_NSAVED", struct pcb, pcb_nsaved);
	off("PCB_ONFAULT", struct pcb, pcb_onfault);
	off("PCB_PSR", struct pcb, pcb_psr);
	off("PCB_RW", struct pcb, pcb_rw);
	off("PCB_SP", struct pcb, pcb_sp);
	off("PCB_PC", struct pcb, pcb_pc);
	off("PCB_UW", struct pcb, pcb_uw);
	off("PCB_WIM", struct pcb, pcb_wim);

	/* interrupt enable register PTE */
	def("IE_REG_PTE", PG_V | PG_W | PG_S | PG_NC | PG_OBIO |
	    ((u_int)INT_ENABLE_REG_PHYSADR >> PGSHIFT));

#ifdef notyet
	/* ZSCC interrupt fields */
	off("ZSC_A", struct zs_softc, sc_a);
	off("ZSC_B", struct zs_softc, sc_b);
/*	off("ZL_WREG", struct zs_line, zl_wreg); */
	off("ZL_TBC", struct zs_line, zl_tbc);
	off("ZL_TBA", struct zs_line, zl_tba);
	off("ZL_RBPUT", struct zs_line, zl_rbput);
	off("ZL_RBUF", struct zs_line, zl_rbuf);
	def("ZSRR1_DO_bit", ffs(ZSRR1_DO) - 1);
#endif
	/* audio trap handler fields */
	def("AUCB_SIZE", AUCB_SIZE);
	off("CB_HEAD", struct aucb, cb_head);
	off("CB_TAIL", struct aucb, cb_tail);
	off("CB_PAUSE", struct aucb, cb_pause);
	off("CB_DATA", struct aucb, cb_data);
	off("CB_DROPS", struct aucb, cb_drops);
	off("CB_PDROPS", struct aucb, cb_drops);
	off("CB_THRESH", struct aucb, cb_thresh);
	off("CB_WAKING", struct aucb, cb_waking);
	off("AU_AMD", struct auio, au_amd);
	off("AU_RB", struct auio, au_rb);
	off("AU_WB", struct auio, au_wb);
	off("AU_STAMP", struct auio, au_stamp);
	off("AMD_IR", struct amd7930, ir);
	off("AMD_BBRB", struct amd7930, bbrb);
	off("AMD_BBTB", struct amd7930, bbtb);

	flush();

	exit(0);
}
