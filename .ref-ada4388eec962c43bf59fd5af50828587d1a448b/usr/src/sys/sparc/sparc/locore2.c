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
 * %sccs.include.redist.c%
 *
 *	@(#)locore2.c	8.4 (Berkeley) %G%
 *
 * from: $Header: locore2.c,v 1.8 92/11/26 03:05:01 mccanne Exp $ (LBL)
 */

/*
 * Primitives which are in locore.s on other machines,
 * but which have no reason to be assembly-coded on SPARC.
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/resourcevar.h>

int	whichqs;

/*
 * Put process p on the run queue indicated by its priority.
 * Calls should be made at splstatclock(), and p->p_stat should be SRUN.
 */
void
setrunqueue(p)
	register struct proc *p;
{
	register struct prochd *q;
	register struct proc *oldlast;
	register int which = p->p_priority >> 2;

	if (p->p_back != NULL)
		panic("setrunqueue");
	q = &qs[which];
	whichqs |= 1 << which;
	p->p_forw = (struct proc *)q;
	p->p_back = oldlast = q->ph_rlink;
	q->ph_rlink = p;
	oldlast->p_forw = p;
}

/*
 * Remove process p from its run queue, which should be the one
 * indicated by its priority.  Calls should be made at splstatclock().
 */
remrq(p)
	register struct proc *p;
{
	register int which = p->p_priority >> 2;
	register struct prochd *q;

	if ((whichqs & (1 << which)) == 0)
		panic("remrq");
	p->p_forw->p_back = p->p_back;
	p->p_back->p_forw = p->p_forw;
	p->p_back = NULL;
	q = &qs[which];
	if (q->ph_link == (struct proc *)q)
		whichqs &= ~(1 << which);
}
