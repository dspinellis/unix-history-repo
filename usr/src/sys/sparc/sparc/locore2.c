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
 *	@(#)locore2.c	7.3 (Berkeley) %G%
 *
 * from: $Header: locore2.c,v 1.7 92/06/20 08:47:28 mccanne Exp $ (LBL)
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
setrq(p)
	register struct proc *p;
{
	register struct prochd *q;
	register struct proc *oldlast;
	register int which = p->p_pri >> 2;

	if (p->p_rlink != NULL)
		panic("setrq");
	q = &qs[which];
	whichqs |= 1 << which;
	p->p_link = (struct proc *)q;
	p->p_rlink = oldlast = q->ph_rlink;
	q->ph_rlink = p;
	oldlast->p_link = p;
}

/*
 * Remove process p from its run queue, which should be the one
 * indicated by its priority.  Calls should be made at splstatclock().
 */
remrq(p)
	register struct proc *p;
{
	register int which = p->p_pri >> 2;
	register struct prochd *q;

	if ((whichqs & (1 << which)) == 0)
		panic("remrq");
	p->p_link->p_rlink = p->p_rlink;
	p->p_rlink->p_link = p->p_link;
	p->p_rlink = NULL;
	q = &qs[which];
	if (q->ph_link == (struct proc *)q)
		whichqs &= ~(1 << which);
}
