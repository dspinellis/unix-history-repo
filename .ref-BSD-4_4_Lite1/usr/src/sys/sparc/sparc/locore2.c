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
 *	@(#)locore2.c	8.4 (Berkeley) 12/10/93
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
