/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)signal.h	7.1 (Berkeley) %G%
 */

/*
 * Machine-dependent signal definitions
 */

typedef int sig_atomic_t;

/*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to restore state properly if
 * a non-standard exit is performed.
 */
struct	sigcontext {
	int	sc_onstack;	/* sigstack state to restore */
	int	sc_mask;	/* signal mask to restore */
	int	sc_pc;		/* pc at time of signal */
	int	sc_regs[34];	/* processor regs 0 to 31, mullo, mullhi */
	int	sc_fpused;	/* fp has been used */
	int	sc_fpregs[33];	/* fp regs 0 to 31 and csr */
	int	sc_fpc_eir;	/* floating point exception instruction reg */
};
