/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kern_proc.c	7.11 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "user.h"
#include "kernel.h"
#include "proc.h"
#include "buf.h"
#include "seg.h"
#include "acct.h"
#include "wait.h"
#include "vm.h"
#include "text.h"
#include "file.h"
#include "../ufs/quota.h"
#include "uio.h"
#include "malloc.h"
#include "mbuf.h"
#include "ioctl.h"
#include "tty.h"

#include "machine/reg.h"
#include "machine/pte.h"
#include "machine/psl.h"

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

/*
 * Locate a process by number
 */
struct proc *
pfind(pid)
	register pid;
{
	register struct proc *p = pidhash[PIDHASH(pid)];

	for (; p; p = p->p_hash)
		if (p->p_pid == pid)
			return (p);
	return ((struct proc *)0);
}

/*
 * Locate a process group by number
 */
struct pgrp *
pgfind(pgid)
	register pid_t pgid;
{
	register struct pgrp *pgrp = pgrphash[PIDHASH(pgid)];

	for (; pgrp; pgrp = pgrp->pg_hforw)
		if (pgrp->pg_id == pgid)
			return (pgrp);
	return ((struct pgrp *)0);
}

/*
 * Move p to a new or existing process group (and session)
 */
pgmv(p, pgid, mksess)
	register struct proc *p;
	pid_t pgid;
{
	register struct pgrp *pgrp = pgfind(pgid);
	register struct proc **pp = &p->p_pgrp->pg_mem;
	register struct proc *cp;
	struct pgrp *opgrp;
	register n;

#ifdef DIAGNOSTIC
	if (pgrp && mksess)	/* firewalls */
		panic("pgmv: setsid into non-empty pgrp");
	if (SESS_LEADER(p))
		panic("pgmv: session leader attempted setpgrp");
#endif
	if (pgrp == NULL) {
		/*
		 * new process group
		 */
#ifdef DIAGNOSTIC
		if (p->p_pid != pgid)
			panic("pgmv: new pgrp and pid != pgid");
#endif
		MALLOC(pgrp, struct pgrp *, sizeof(struct pgrp), M_PGRP,
		       M_WAITOK);
		if (mksess) {
			register struct session *sess;
			/*
			 * new session
			 */
			MALLOC(sess, struct session *, sizeof(struct session),
				M_SESSION, M_WAITOK);
			sess->s_leader = p;
			sess->s_count = 1;
			sess->s_ttyvp = NULL;
			sess->s_ttyp = NULL;
			p->p_flag &= ~SCTTY;
			pgrp->pg_session = sess;
#ifdef DIAGNOSTIC
			if (p != u.u_procp)
				panic("pgmv: mksession and p != u.u_procp");
#endif
		} else {
			pgrp->pg_session = p->p_session;
			pgrp->pg_session->s_count++;
		}
		pgrp->pg_id = pgid;
		pgrp->pg_hforw = pgrphash[n=PIDHASH(pgid)];
		pgrphash[n] = pgrp;
		pgrp->pg_jobc = 0;
		pgrp->pg_mem = NULL;
	} else if (pgrp == p->p_pgrp)
		return;
	/*
	 * adjust eligibility of affected pgrps to participate in job control
	 */
	fixjobc(p, 0);
	/*
	 * unlink p from old process group
	 */
	for (; *pp; pp = &(*pp)->p_pgrpnxt)
		if (*pp == p) {
			*pp = p->p_pgrpnxt;
			goto done;
		}
	panic("pgmv: can't find p on old pgrp");
done:
	/*
	 * link into new one
	 */
	p->p_pgrpnxt = pgrp->pg_mem;
	pgrp->pg_mem = p;
	opgrp = p->p_pgrp;
	p->p_pgrp = pgrp;
	/*
	 * adjust eligibility of affected pgrps to participate in job control
	 */
	fixjobc(p, 1);
	/*
	 * delete old if empty
	 */
	if (!opgrp->pg_mem)
		pgdelete(opgrp);
}

/*
 * remove process from process group
 */
pgrm(p)
	register struct proc *p;
{
	register struct proc **pp = &p->p_pgrp->pg_mem;

	for (; *pp; pp = &(*pp)->p_pgrpnxt)
		if (*pp == p) {
			*pp = p->p_pgrpnxt;
			goto done;
		}
	panic("pgrm: can't find p in pgrp");
done:
	if (!p->p_pgrp->pg_mem)
		pgdelete(p->p_pgrp);
	p->p_pgrp = 0;
}

/*
 * delete a process group
 */
pgdelete(pgrp)
	register struct pgrp *pgrp;
{
	register struct pgrp **pgp = &pgrphash[PIDHASH(pgrp->pg_id)];

	if (pgrp->pg_session->s_ttyp != NULL && 
	    pgrp->pg_session->s_ttyp->t_pgrp == pgrp)
		pgrp->pg_session->s_ttyp->t_pgrp = NULL;
	for (; *pgp; pgp = &(*pgp)->pg_hforw)
		if (*pgp == pgrp) {
			*pgp = pgrp->pg_hforw;
			goto done;
		}
	panic("pgdelete: can't find pgrp on hash chain");
done:
	if (--pgrp->pg_session->s_count == 0)
		FREE(pgrp->pg_session, M_SESSION);
	FREE(pgrp, M_PGRP);
}

/*
 * Adjust pgrp jobc counter.
 * flag == 0 => p is leaving current state.
 * flag == 1 => p is entering current state.
 */
fixjobc(p, flag)
	register struct proc *p;
	register flag;
{
	register struct pgrp *mypgrp = p->p_pgrp, *hispgrp;
	register struct session *mysession = mypgrp->pg_session;
	register struct proc *qp;

	if ((hispgrp = p->p_pptr->p_pgrp) != mypgrp &&
	    hispgrp->pg_session == mysession)
		if (flag)
			mypgrp->pg_jobc++;
		else if (--mypgrp->pg_jobc == 0) {
			int deliver = 0;

			sigstopped:
			for (qp = mypgrp->pg_mem; qp != NULL; 
			     qp = qp->p_pgrpnxt)
				if (deliver) {
					psignal(qp, SIGHUP);
					psignal(qp, SIGCONT);
				} else if (qp->p_stat == SSTOP) {
					deliver++;
					goto sigstopped;
				}
		}

	for (p = p->p_cptr; p != NULL; p = p->p_osptr)
		if ((hispgrp = p->p_pgrp) != mypgrp &&
		    hispgrp->pg_session == mysession &&
		    p->p_stat != SZOMB)
			if (flag)
				hispgrp->pg_jobc++;
			else if (--hispgrp->pg_jobc == 0) {
				int deliver = 0;

				sigstopped2:
				for (qp = hispgrp->pg_mem; qp != NULL; 
				     qp = qp->p_pgrpnxt)
					if (deliver) {
						psignal(qp, SIGHUP);
						psignal(qp, SIGCONT);
					} else if (qp->p_stat == SSTOP) {
						deliver++;
						goto sigstopped2;
					}
			}
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

#ifdef debug
/* DEBUG */
pgrpdump()
{
	register struct pgrp *pgrp;
	register struct proc *p;
	register i;

	for (i=0; i<PIDHSZ; i++) {
		if (pgrphash[i]) {
		  printf("\tindx %d\n", i);
		  for (pgrp=pgrphash[i]; pgrp; pgrp=pgrp->pg_hforw) {
		    printf("\tpgrp %x, pgid %d, sess %x, sesscnt %d, mem %x\n",
			pgrp, pgrp->pg_id, pgrp->pg_session,
			pgrp->pg_session->s_count, pgrp->pg_mem);
		    for (p=pgrp->pg_mem; p; p=p->p_pgrpnxt) {
			printf("\t\tpid %d addr %x pgrp %x\n", 
				p->p_pid, p, p->p_pgrp);
		    }
		  }

		}
	}
}
#endif /* debug */
