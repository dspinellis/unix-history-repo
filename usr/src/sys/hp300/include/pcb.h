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
 * from: Utah $Hdr: pcb.h 1.14 91/03/25$
 *
 *	@(#)pcb.h	7.7 (Berkeley) %G%
 */

#include <machine/frame.h>

/*
 * HP300 process control block
 */
struct pcb
{
	short	pcb_flags;	/* misc. process flags */
	short	pcb_ps; 	/* processor status word */
	int	pcb_ustp;	/* user segment table pointer */
	int	pcb_usp;	/* user stack pointer */
	int	pcb_regs[12];	/* D2-D7, A2-A7 */
	caddr_t	pcb_onfault;	/* for copyin/out faults */
	struct	fpframe pcb_fpregs; /* 68881/2 context save area */
};

/*
 * The pcb is augmented with machine-dependent additional data for
 * core dumps. For the hp300, this includes an HP-UX exec header
 * which is dumped for HP-UX processes.
 */
struct md_coredump {
	int	md_exec[16];	/* exec structure for HP-UX core dumps */
};
