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
 *	@(#)timerreg.h	8.1 (Berkeley) 6/11/93
 *
 * from: $Header: timerreg.h,v 1.7 92/11/26 03:05:09 leres Exp $ (LBL)
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
