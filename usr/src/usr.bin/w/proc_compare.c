/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)proc_compare.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/proc.h>

#include "extern.h"

/*
 * Returns 1 if p2 is "better" than p1
 *
 * The algorithm for picking the "interesting" process is thus:
 *
 *	1) Only foreground processes are eligible - implied.
 *	2) Runnable processes are favored over anything else.  The runner
 *	   with the highest cpu utilization is picked (p_estcpu).  Ties are
 *	   broken by picking the highest pid.
 *	3) The sleeper with the shortest sleep time is next.  With ties,
 *	   we pick out just "short-term" sleepers (P_SINTR == 0).
 *	4) Further ties are broken by picking the highest pid.
 *
 * If you change this, be sure to consider making the change in the kernel
 * too (^T in kern/tty.c).
 *
 * TODO - consider whether pctcpu should be used.
 */

#define ISRUN(p)	(((p)->p_stat == SRUN) || ((p)->p_stat == SIDL))
#define TESTAB(a, b)    ((a)<<1 | (b))
#define ONLYA   2
#define ONLYB   1
#define BOTH    3

int
proc_compare(p1, p2)
	register struct proc *p1, *p2;
{

	if (p1 == NULL)
		return (1);
	/*
	 * see if at least one of them is runnable
	 */
	switch (TESTAB(ISRUN(p1), ISRUN(p2))) {
	case ONLYA:
		return (0);
	case ONLYB:
		return (1);
	case BOTH:
		/*
		 * tie - favor one with highest recent cpu utilization
		 */
		if (p2->p_estcpu > p1->p_estcpu)
			return (1);
		if (p1->p_estcpu > p2->p_estcpu)
			return (0);
		return (p2->p_pid > p1->p_pid);	/* tie - return highest pid */
	}
	/*
 	 * weed out zombies
	 */
	switch (TESTAB(p1->p_stat == SZOMB, p2->p_stat == SZOMB)) {
	case ONLYA:
		return (1);
	case ONLYB:
		return (0);
	case BOTH:
		return (p2->p_pid > p1->p_pid); /* tie - return highest pid */
	}
	/*
	 * pick the one with the smallest sleep time
	 */
	if (p2->p_slptime > p1->p_slptime)
		return (0);
	if (p1->p_slptime > p2->p_slptime)
		return (1);
	/*
	 * favor one sleeping in a non-interruptible sleep
	 */
	if (p1->p_flag & P_SINTR && (p2->p_flag & P_SINTR) == 0)
		return (1);
	if (p2->p_flag & P_SINTR && (p1->p_flag & P_SINTR) == 0)
		return (0);
	return (p2->p_pid > p1->p_pid);		/* tie - return highest pid */
}
