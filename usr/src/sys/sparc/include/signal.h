/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
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
 * %sccs.include.redist.c%
 *
 *	@(#)signal.h	7.4 (Berkeley) %G%
 *
 * from: $Header: signal.h,v 1.5 92/11/26 02:04:46 torek Exp $
 */

#ifndef LOCORE
typedef int sig_atomic_t;

/*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to restore state properly if
 * a non-standard exit is performed.
 *
 * All machines must have an sc_onstack and sc_mask.
 */
struct sigcontext {
	int	sc_onstack;		/* sigstack state to restore */
	int	sc_mask;		/* signal mask to restore */
	/* begin machine dependent portion */
	int	sc_sp;			/* %sp to restore */
	int	sc_pc;			/* pc to restore */
	int	sc_npc;			/* npc to restore */
	int	sc_psr;			/* psr to restore */
	int	sc_g1;			/* %g1 to restore */
	int	sc_o0;			/* %o0 to restore */
};
#else /* LOCORE */
#define	SC_SP_OFFSET	8
#define	SC_PC_OFFSET	12
#define	SC_NPC_OFFSET	16
#define	SC_PSR_OFFSET	20
#define	SC_G1_OFFSET	24
#define	SC_O0_OFFSET	28
#endif /* LOCORE */

/*
 * `Code' arguments to signal handlers.  The names, and the funny numbering.
 * are defined so as to match up with what SunOS uses; I have no idea why
 * they did the numbers that way, except maybe to match up with the 68881.
 */
#define	FPE_INTOVF_TRAP		0x01	/* integer overflow */
#define	FPE_INTDIV_TRAP		0x14	/* integer divide by zero */
#define	FPE_FLTINEX_TRAP	0xc4	/* inexact */
#define	FPE_FLTDIV_TRAP		0xc8	/* divide by zero */
#define	FPE_FLTUND_TRAP		0xcc	/* underflow */
#define	FPE_FLTOPERR_TRAP	0xd0	/* operand error */
#define	FPE_FLTOVF_TRAP		0xd4	/* overflow */
