/*	@(#)sleep.c	4.2 (Berkeley) %G%	*/

#include <signal.h>
#include <setjmp.h>
#include <time.h>

static jmp_buf jmp;

#define	mask(s)	(1<<((s)-1))
#define	setvec(vec, a) \
	vec.sv_handler = a; vec.sv_mask = vec.sv_onstack = 0

sleep(n)
	unsigned n;
{
	int sleepx(), omask;
	struct itimerval itv, oitv;
	register struct itimerval *itp = &itv;
	struct sigvec vec, ovec;

	if (n == 0)
		return;
	timerclear(&itp->it_interval);
	timerclear(&itp->it_value);
	if (setitimer(ITIMER_REAL, itp, &oitv) < 0)
		return;
	setvec(ovec, SIG_DFL);
	if (setjmp(jmp)) {
		(void) sigvec(SIGALRM, &ovec, (struct sigvec *)0);
		(void) setitimer(ITIMER_REAL, &oitv, (struct itimerval *)0);
		return;
	}
	omask = sigblock(0);
	itp->it_value.tv_sec = n;
	if (timerisset(&oitv.it_value)) {
		if (timercmp(&oitv.it_value, &itp->it_value, >))
			oitv.it_value.tv_sec -= itp->it_value.tv_sec;
		else {
			itp->it_value = oitv.it_value;
			/*
			 * Set the reset value to the smallest possible,
			 * the system will round it to the clock resolution.
			 */
			oitv.it_value.tv_sec = 0;
			oitv.it_value.tv_usec = 1;
		}
	}
	setvec(vec, sleepx);
	(void) sigvec(SIGALRM, &vec, &ovec);
	if (setitimer(ITIMER_REAL, itp, (struct itimerval *)0) < 0)
		longjmp(jmp, 1);
	sigpause(omask &~ mask(SIGALRM));
	/*NOTREACHED*/
}

static
sleepx()
{

	longjmp(jmp, 1);
}
