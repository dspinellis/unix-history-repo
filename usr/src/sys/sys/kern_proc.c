/*	kern_proc.c	6.1	83/07/29	*/

#include "../machine/reg.h"
#include "../machine/pte.h"
#include "../machine/psl.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/proc.h"
#include "../h/buf.h"
#include "../h/inode.h"
#include "../h/seg.h"
#include "../h/acct.h"
#include "../h/wait.h"
#include "../h/vm.h"
#include "../h/text.h"
#include "../h/file.h"
#include "../h/quota.h"
#include "../h/uio.h"
#include "../h/mbuf.h"
#include "../h/nami.h"

spgrp(top, npgrp)
	register struct proc *top;
{
	register struct proc *pp, *p;
	int f = 0;

	for (p = top; npgrp == -1 || u.u_uid == p->p_uid ||
	    !u.u_uid || inferior(p); p = pp) {
		if (npgrp == -1) {
#define	bit(a)	(1<<(a-1))
			p->p_sig &= ~(bit(SIGTSTP)|bit(SIGTTIN)|bit(SIGTTOU));
		} else
			p->p_pgrp = npgrp;
		f++;
		/*
		 * Search for children.
		 */
		for (pp = proc; pp < procNPROC; pp++)
			if (pp->p_pptr == p)
				goto cont;
		/*
		 * Search for siblings.
		 */
		for (; p != top; p = p->p_pptr)
			for (pp = p + 1; pp < procNPROC; pp++)
				if (pp->p_pptr == p->p_pptr)
					goto cont;
		break;
	cont:
		;
	}
	return (f);
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
