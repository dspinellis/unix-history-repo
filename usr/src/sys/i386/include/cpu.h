/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cpu.h	5.6 (Berkeley) %G%
 */

/*
 * Definitions unique to i386 cpu support.
 */
#include <machine/frame.h>
#include <machine/segments.h>

/*
 * definitions of cpu-dependent requirements
 * referenced in generic code
 */
#undef	COPY_SIGCODE		/* don't copy sigcode above user stack in exec */

/*
 * function vs. inline configuration;
 * these are defined to get generic functions
 * rather than inline or machine-dependent implementations
 */
#define	NEED_MINMAX		/* need {,i,l,ul}{min,max} functions */
#define	NEED_FFS		/* need ffs function */
#define	NEED_BCMP		/* need bcmp function */
#define	NEED_STRLEN		/* need strlen function */

#define	cpu_exec(p)	/* nothing */
#define cpu_setstack(p, ap) \
	(p)->p_md.md_regs[SP] = ap

/*
 * Arguments to hardclock, softclock and gatherstats
 * encapsulate the previous machine state in an opaque
 * clockframe; for now, use generic intrframe.
 */
typedef struct intrframe clockframe;

#define	CLKF_USERMODE(framep)	(ISPL((framep)->if_cs) == SEL_UPL)
#define	CLKF_BASEPRI(framep)	((framep)->if_ppl == 0)
#define	CLKF_PC(framep)		((framep)->if_eip)

#define	resettodr()	/* no todr to set */

/*
 * Preempt the current process if in interrupt from user mode,
 * or after the current trap/syscall if in system mode.
 */
#define	need_resched()	{ want_resched++; aston(); }

/*
 * Give a profiling tick to the current process from the softclock
 * interrupt.  On tahoe, request an ast to send us through trap(),
 * marking the proc as needing a profiling tick.
 */
#define	profile_tick(p, framep)	{ (p)->p_flag |= SOWEUPC; aston(); }

/*
 * Notify the current process (p) that it has a signal pending,
 * process as soon as possible.
 */
#define	signotify(p)	aston()

#define aston() (astpending++)

int	astpending;		/* need to trap before returning to user mode */
int	want_resched;		/* resched() was called */

/*
 * Kinds of processor
 */

#define	CPU_386SX	0
#define	CPU_386		1
#define	CPU_486SX	2
#define	CPU_486		3
#define	CPU_586		4
