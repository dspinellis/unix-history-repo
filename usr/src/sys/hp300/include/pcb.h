/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: pcb.h 1.13 89/04/23$
 *
 *	@(#)pcb.h	7.3 (Berkeley) %G%
 */

#include <hp300/include/frame.h>

/*
 * HP300 process control block
 */
struct pcb
{
	short	pcb_flags;	/* misc. process flags (+0) */
	short	pcb_ps; 	/* processor status word (+2) */
	int	pcb_ustp;	/* user segment table pointer (+4) */
	int	pcb_usp;	/* user stack pointer (+8) */
	int	pcb_regs[12];	/* D0-D7, A0-A7 (+C) */
	int	pcb_pad[5];
	int	pcb_cmap2;	/* temporary copy PTE (+50) */
	int	*pcb_sswap;	/* saved context for swap return (+54) */
	short	pcb_sigc[12];	/* signal trampoline code (+58) */
	caddr_t	pcb_onfault;	/* for copyin/out faults (+70) */
	struct fpframe pcb_fpregs; /* 68881/2 context save area (+74) */
	int	pcb_exec[16];	/* exec structure for core dumps (+1B8) */
	int	pcb_res[2];	/* reserved for future expansion (+1F8) */
};

/* flags */

#define	PCB_AST		0x0001	/* async trap pending */
#define PCB_HPUXMMAP	0x0010	/* VA space is multiple mapped */
#define PCB_HPUXTRACE	0x0020	/* being traced by an HPUX process */
#define PCB_HPUXBIN	0x0040	/* loaded from an HPUX format binary */
				/* note: does NOT imply SHPUX */

#define aston() \
	{ \
		u.u_pcb.pcb_flags |= PCB_AST; \
	}

#define astoff() \
	{ \
		u.u_pcb.pcb_flags &= ~PCB_AST; \
	}

#define astpend() \
	(u.u_pcb.pcb_flags & PCB_AST)
