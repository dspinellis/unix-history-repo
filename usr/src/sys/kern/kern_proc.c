/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_proc.c	6.6 (Berkeley) %G%
 */

#include "../machine/reg.h"
#include "../machine/pte.h"
#include "../machine/psl.h"

#include "param.h"
#include "systm.h"
#include "map.h"
#include "dir.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "buf.h"
#include "inode.h"
#include "seg.h"
#include "acct.h"
#include "wait.h"
#include "vm.h"
#include "text.h"
#include "file.h"
#include "quota.h"
#include "uio.h"
#include "mbuf.h"

/*
 * Change the process group of top and all descendents to npgrp.
 * If npgrp is -1, instead clear any pending stops.
 */
spgrp(top, npgrp)
	struct proc *top;
{
	register struct proc *p;
	int f = 0;

	p = top;
	for (;;) {
		if (npgrp == -1)
			p->p_sig &=
			  ~(sigmask(SIGTSTP)|sigmask(SIGTTIN)|sigmask(SIGTTOU));
		else
			p->p_pgrp = npgrp;
		f++;
		/*
		 * If this process has children, descend to them next,
		 * otherwise do any siblings, and if done with this level,
		 * follow back up the tree (but not past top).
		 */
		if (p->p_cptr)
			p = p->p_cptr;
		else if (p == top)
			return (f);
		else if (p->p_osptr)
			p = p->p_osptr;
		else for (;;) {
			p = p->p_pptr;
			if (p == top)
				return (f);
if (p == &proc[1])
	panic("spgrp");
			if (p->p_osptr) {
				p = p->p_osptr;
				break;
			}
		}
	}
}

/*
 * Is p an inferior of the current process?
 */
inferior(p)
	register struct proc *p;
{

	for (; p != u.u_procp; p = p->p_pptr)
		if (p->p_ppid == 0)
			return (0);
	return (1);
}

struct proc *
pfind(pid)
	int pid;
{
	register struct proc *p;

	for (p = &proc[pidhash[PIDHASH(pid)]]; p != &proc[0]; p = &proc[p->p_idhash])
		if (p->p_pid == pid)
			return (p);
	return ((struct proc *)0);
}

/*
 * init the process queues
 */
pqinit()
{
	register struct proc *p;

	/*
	 * most procs are initially on freequeue
	 *	nb: we place them there in their "natural" order.
	 */

	freeproc = NULL;
	for (p = procNPROC; --p > proc; freeproc = p)
		p->p_nxt = freeproc;

	/*
	 * but proc[0] is special ...
	 */

	allproc = p;
	p->p_nxt = NULL;
	p->p_prev = &allproc;

	zombproc = NULL;
}
