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
 *	@(#)pcb.h	7.4 (Berkeley) %G%
 */

#include <machine/frame.h>

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
	int	pcb_cmap2;	/* temporary copy PTE */
	caddr_t	pcb_onfault;	/* for copyin/out faults */
	struct	fpframe pcb_fpregs; /* 68881/2 context save area */
	int	pcb_exec[16];	/* exec structure for core dumps */
};

/* flags */

#define PCB_HPUXMMAP	0x0010	/* VA space is multiple mapped */
#define PCB_HPUXTRACE	0x0020	/* being traced by an HPUX process */
#define PCB_HPUXBIN	0x0040	/* loaded from an HPUX format binary */
				/* note: does NOT imply SHPUX */
