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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)timerreg.h	7.3 (Berkeley) %G%
 *
 * from: $Header: timerreg.h,v 1.6 92/07/07 04:56:09 leres Exp $ (LBL)
 */

/*
 * Sun-4c counter/timer registers.  The timers are implemented within
 * the cache chip (!).  The counter and limit fields below could be
 * defined as:
 *
 *	struct {
 *		u_int	t_limit:1,	// limit reached
 *			t_usec:21,	// counter value in microseconds
 *			t_mbz:10;	// always zero
 *	};
 *
 * but this is more trouble than it is worth.
 *
 * These timers work in a rather peculiar fashion.  Most clock counters
 * run to 0 (as, e.g., on the VAX, where the ICR counts up to 0 from a
 * large unsigned number).  On the Sun-4c, it counts up to a limit.  But
 * for some reason, when it reaches the limit, it resets to 1, not 0.
 * Thus, if the limit is set to 4, the counter counts like this:
 *
 *	1, 2, 3, 1, 2, 3, ...
 *
 * and if we want to divide by N we must set the limit register to N+1.
 */
#ifndef LOCORE
struct timer {
	int	t_counter;		/* counter reg */
	int	t_limit;		/* limit reg */
};

struct timerreg {
	struct	timer t_c10;		/* counter that interrupts at ipl 10 */
	struct	timer t_c14;		/* counter that interrupts at ipl 14 */
};
#endif

#define	TMR_LIMIT	0x80000000	/* counter reached its limit */
#define	TMR_SHIFT	10		/* shift to obtain microseconds */
#define	TMR_MASK	0x1fffff	/* 21 bits */

/* Compute a limit that causes the timer to fire every n microseconds. */
#define	tmr_ustolim(n)	(((n) + 1) << TMR_SHIFT)

#include <sparc/sparc/vaddrs.h>
#define	TIMERREG	((volatile struct timerreg *)TIMERREG_VA)
