/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: pcb.h 1.13 89/04/23$
 *
 *	@(#)pcb.h	7.2 (Berkeley) %G%
 */

/*
 * PMAX process control block
 */
struct pcb
{
	int	pcb_regs[69];	/* saved CPU and floating point registers */
	label_t	pcb_context;	/* kernel context for resume */
	int	pcb_onfault;	/* for copyin/copyout faults */
};

/*
 * The pcb is augmented with machine-dependent additional data for
 * core dumps. For the PMAX, there is nothing to add.
 */
struct md_coredump {
	long	md_pad[8];
};
