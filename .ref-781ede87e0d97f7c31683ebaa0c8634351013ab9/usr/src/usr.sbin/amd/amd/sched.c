/*
 * $Id: sched.c,v 5.2 90/06/23 22:19:58 jsp Rel $
 *
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sched.c	5.1 (Berkeley) %G%
 */

/*
 * Process scheduler
 */

#include "am.h"
#include <sys/signal.h>
#include WAIT
#include <setjmp.h>
extern jmp_buf select_intr;
extern int select_intr_valid;

typedef struct pjob pjob;
struct pjob {
	qelem hdr;			/* Linked list */
	int pid;			/* Process ID of job */
	cb_fun cb_fun;			/* Callback function */
	voidp cb_closure;		/* Closure for callback */
	union wait w;			/* Status filled in by sigchld */
	voidp wchan;			/* Wait channel */
};

extern qelem proc_list_head;
qelem proc_list_head = { &proc_list_head, &proc_list_head };
extern qelem proc_wait_list;
qelem proc_wait_list = { &proc_wait_list, &proc_wait_list };

int task_notify_todo;

void ins_que(elem, pred)
qelem *elem, *pred;
{
	qelem *p = pred->q_forw;
	elem->q_back = pred;
	elem->q_forw = p;
	pred->q_forw = elem;
	p->q_back = elem;
}

void rem_que(elem)
qelem *elem;
{
	qelem *p = elem->q_forw;
	qelem *p2 = elem->q_back;
	p2->q_forw = p;
	p->q_back = p2;
}

static pjob *sched_job(cf, ca)
cb_fun cf;
voidp ca;
{
	pjob *p = ALLOC(pjob);

	p->cb_fun = cf;
	p->cb_closure = ca;

	/*
	 * Now place on wait queue
	 */
	ins_que(&p->hdr, &proc_wait_list);

	return p;
}

void run_task(tf, ta, cf, ca)
task_fun tf;
voidp ta;
cb_fun cf;
voidp ca;
{
	pjob *p = sched_job(cf, ca);
	int mask;

	p->wchan = (voidp) p;

	mask = sigblock(sigmask(SIGCHLD));

	if (p->pid = background()) {
		sigsetmask(mask);
		return;
	}

	exit((*tf)(ta));
	/* firewall... */
	abort();
}

/*
 * Schedule a task to be run when woken up
 */
void sched_task(cf, ca, wchan)
cb_fun cf;
voidp ca;
voidp wchan;
{
	/*
	 * Allocate a new task
	 */
	pjob *p = sched_job(cf, ca);
#ifdef DEBUG
	/*dlog("sleep(%#x)", wchan);*/
#endif /* DEBUG */
	p->wchan = wchan;
	p->pid = 0;
	bzero((voidp) &p->w, sizeof(p->w));
}

static void wakeupjob(p)
pjob *p;
{
	rem_que(&p->hdr);
	ins_que(&p->hdr, &proc_list_head);
	task_notify_todo++;
}

void wakeup(wchan)
voidp wchan;
{
	pjob *p, *p2;

	if (!foreground)
		return;

#ifdef DEBUG
	/*dlog("wakeup(%#x)", wchan);*/
#endif /* DEBUG */
	/*
	 * Can't user ITER() here because
	 * wakeupjob() juggles the list.
	 */
	for (p = FIRST(pjob, &proc_wait_list);
			p2 = NEXT(pjob, p), p != HEAD(pjob, &proc_wait_list);
			p = p2) {
		if (p->wchan == wchan)
			wakeupjob(p);
	}
}

void wakeup_task(rc, term, cl)
int rc;
int term;
voidp cl;
{
	wakeup(cl);
}

/*ARGSUSED*/

void sigchld(sig)
int sig;
{
	union wait w;
	int pid;

#ifdef SYS5_SIGNALS
	if ((pid = wait(&w)) > 0) {
#else
	while ((pid = wait3(&w, WNOHANG, (union wait *) 0)) > 0) {
#endif /* SYS5_SIGNALS */
		pjob *p, *p2;

		if (WIFSIGNALED(w))
			plog(XLOG_ERROR, "Process %d exited with signal %d",
				pid, w.w_termsig);
#ifdef DEBUG
		else
			dlog("Process %d exited with status %d",
				pid, w.w_retcode);
#endif /* DEBUG */

		for (p = FIRST(pjob, &proc_wait_list);
				p2 = NEXT(pjob, p), p != HEAD(pjob, &proc_wait_list);
				p = p2) {
			if (p->pid == pid) {
				p->w = w;
				wakeupjob(p);
				break;
			}
		}

#ifdef DEBUG
		if (p) ; else dlog("can't locate task block for pid %d", pid);
#endif /* DEBUG */
	}

#ifdef SYS5_SIGNALS
	signal(sig, sigchld);
#endif /* SYS5_SIGNALS */
	if (select_intr_valid)
		longjmp(select_intr, sigchld);
}

/*
 * Run any pending tasks.
 * This must be called with SIGCHLD disabled
 */
void task_notify(P_void)
{
	/*
	 * Keep taking the first item off the list and processing it.
	 *
	 * Done this way because the the callback can, quite reasonably,
	 * queue a new task, so no local reference into the list can be
	 * held here.
	 */
	while (FIRST(pjob, &proc_list_head) != HEAD(pjob, &proc_list_head)) {
		pjob *p = FIRST(pjob, &proc_list_head);
		rem_que(&p->hdr);
		/*
		 * This job has completed
		 */
		--task_notify_todo;

		/*
		 * Do callback if it exists
		 */
		if (p->cb_fun)
			(*p->cb_fun)(p->w.w_retcode,
				p->w.w_termsig, p->cb_closure);

		free(p);
	}
}
