/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)proc_compare.c	5.3 (Berkeley) 6/26/90";
#endif /* not lint */

/*
 * Returns 1 if p2 is more active than p1
 *
 * The algorithm for picking the "more active" process is thus:
 *
 *	1) Runnable processes are favored over anything
 *	   else.  The runner with the highest cpu
 *	   utilization is picked (p_cpu).  Ties are
 *	   broken by picking the highest pid.
 *	2) Next, the sleeper with the shortest sleep
 *	   time is favored.  With ties, we pick out
 *	   just short-term sleepers (p_pri <= PZERO).
 *	   Further ties are broken by picking the highest
 *	   pid.
 *
 *	NOTE - if you change this, be sure to consider making
 *	   the change in the kernel too (^T in kern/tty.c).
 *
 *	TODO - consider whether pctcpu should be used
 *
 */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/proc.h>

#define isrun(p)	(((p)->p_stat == SRUN) || ((p)->p_stat == SIDL))

#define	TESTAB(a, b)	((a)<<1 | (b))
#define	ONLYA	0x10
#define	ONLYB	0x01
#define	BOTH	0x11

proc_compare(p1, p2)
	register struct proc *p1, *p2;
{

	if (p1 == NULL)
		return (1);
	/*
	 * see if at least one of them is runnable
	 */
	switch (TESTAB(isrun(p1), isrun(p2))) {
	case ONLYA:
		return (1);
	case ONLYB:
		return (0);
	case BOTH:
		/*
		 * tie - favor one with highest recent cpu utilization
		 */
		if (p2->p_cpu > p1->p_cpu)
			return (1);
		if (p1->p_cpu > p2->p_cpu)
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
		return (p2->p_pid > p1->p_pid);	/* tie - return highest pid */
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
	 if (p1->p_flag&SSINTR && (p2->p_flag&SSINTR) == 0)
		 return (1);
	 if (p2->p_flag&SSINTR && (p1->p_flag&SSINTR) == 0)
		 return (0);
	return(p2->p_pid > p1->p_pid);		/* tie - return highest pid */
}
