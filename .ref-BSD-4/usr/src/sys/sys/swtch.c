/*	swtch.c	4.1	11/9/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/vm.h"
#include "../h/pte.h"

/*
 * PORTABLE DEFINITIONS OF setrq(), remrq() and swtch().
 * These are primitives in locore.s on the VAX.
 */

#ifndef FASTVAX
/*
 * when you are sure that it
 * is impossible to get the
 * 'proc on q' diagnostic, the
 * diagnostic loop can be removed.
 */
setrq(p)
struct proc *p;
{
	register struct proc *q;
	register s;

	s = spl6();
	for(q=runq; q!=NULL; q=q->p_link)
		if(q == p) {
			printf("proc on q\n");
			asm("halt");
			goto out;
		}
	p->p_link = runq;
	runq = p;
out:
	splx(s);
}

/*
 * Remove runnable job from run queue.
 * This is done when a runnable job is swapped
 * out so that it won't be selected in swtch().
 * It will be reinserted in the runq with setrq
 * when it is swapped back in.
 */
remrq(p)
	register struct proc *p;
{
	register struct proc *q;
	int s;

	s = spl6();
	if (p == runq)
		runq = p->p_link;
	else {
		for (q = runq; q; q = q->p_link)
			if (q->p_link == p) {
				q->p_link = p->p_link;
				goto done;
			}
		panic("remque");
done:
		;
	}
	splx(s);
}

/*
 * This routine is called to reschedule the CPU.
 * if the calling process is not in RUN state,
 * arrangements for it to restart must have
 * been made elsewhere, usually by calling via sleep.
 * There is a race here. A process may become
 * ready after it has been examined.
 * In this case, idle() will be called and
 * will return in at most 1HZ time.
 * i.e. its not worth putting an spl() in.
 */
swtch()
{
	register n;
	register struct proc *p, *q, *pp, *pq;

	noproc = 1;
loop:
	(void) spl6();
	runrun = 0;
	pp = NULL;
	q = NULL;
	n = 128;
	/*
	 * Search for highest-priority runnable process
	 */
	for(p=runq; p!=NULL; p=p->p_link) {
		if(p->p_pri < n) {
			pp = p;
			pq = q;
			n = p->p_pri;
		}
		q = p;
	}
	/*
	 * If no process is runnable, idle.
	 */
	p = pp;
	if(p == NULL) {
		idle();
		goto loop;
	}
	q = pq;
	if(q == NULL)
		runq = p->p_link;
	else
		q->p_link = p->p_link;
	curpri = n;
	(void) spl0();
	cnt.v_swtch++;
	noproc = 0;
	if (p != u.u_procp)
		resume(pcbb(p));
}
#endif
