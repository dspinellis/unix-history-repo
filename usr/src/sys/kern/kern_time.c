/*	kern_time.c	5.3	82/09/04	*/

#include "../h/param.h"
#include "../h/dir.h"		/* XXX */
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"

gettimeofday()
{
	register struct a {
		struct	timeval *tp;
		struct	timezone *tzp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;

	microtime(&atv);
	if (copyout((caddr_t)&atv, (caddr_t)uap->tp, sizeof (atv))) {
		u.u_error = EFAULT;
		return;
	}
	if (uap->tzp == 0)
		return;
	if (copyout((caddr_t)&tz, uap->tzp, sizeof (tz))) {
		u.u_error = EFAULT;
		return;
	}
}

settimeofday()
{
	register struct a {
		struct timeval *tv;
		struct timezone *tzp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;
	struct timezone atz;

	if (copyin((caddr_t)uap->tv, (caddr_t)&atv, sizeof (struct timeval))) {
		u.u_error = EFAULT;
		return;
	}
	if (suser()) {
		struct timeval tdelta;

		tdelta = atv;

		timevalsub(&tdelta, &time);
		timevaladd(&boottime, &tdelta);
		time = atv;
		clockset();
	}
	if (uap->tzp) {
		if (copyin((caddr_t)uap->tzp, (caddr_t)&atz, sizeof (atz))) {
			u.u_error = EFAULT;
			return;
		}
		/* XXX */
	}
}

timevaladd(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec += t2->tv_sec;
	t1->tv_usec += t2->tv_sec;
	timevalfix(t1);
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_sec;
	timevalfix(t1);
}

timevalfix(t1)
	struct timeval *t1;
{

	if (t1->tv_usec < 0) {
		t1->tv_sec--;
		t1->tv_usec += 1000000;
	}
	if (t1->tv_usec >= 1000000) {
		t1->tv_sec++;
		t1->tv_usec -= 1000000;
	}
}

getitimer()
{
	register struct a {
		u_int	which;
		struct	itimerval *itv;
	} *uap = (struct a *)u.u_ap;
	register struct itimerval *itp;
	int s;

	if (uap->which > 2) {
		u.u_error = EINVAL;
		return;
	}
	if (uap->which == ITIMER_REAL)
		itp = &u.u_procp->p_realtimer;
	else
		itp = &u.u_timer[uap->which];
	s = spl7();
	if (copyout((caddr_t)itp, uap->itv, sizeof (struct itimerval))) {
		u.u_error = EFAULT;
		goto bad;
	}
bad:
	splx(s);
}

setitimer()
{
	register struct a {
		u_int	which;
		struct	itimerval *itv;
	} *uap = (struct a *)u.u_ap;
	struct itimerval aitv;
	int s;

	s = spl7();
	if (uap->which > 2) {
		u.u_error = EINVAL;
		goto bad;
	}
	if (copyin((caddr_t)uap->itv, (caddr_t)&aitv,
	    sizeof (struct itimerval))) {
		u.u_error = EFAULT;
		goto bad;
	}
	u.u_timer[uap->which] = aitv;
	if (uap->which == ITIMER_REAL)
		u.u_procp->p_realtimer = aitv;
bad:
	splx(s);
	return;
}

getandsetitimer()
{
	int s = spl7();

	getitimer();
	if (u.u_error == 0) {
		u.u_ap[1] = u.u_ap[2];
		setitimer();
	}
	splx(s);
}

itimerdecr(itp, usec)
	register struct itimerval *itp;
	int usec;
{

	while (itp->itimer_value.tv_usec < usec) {
		if (itp->itimer_value.tv_sec == 0)
			goto expire;
		itp->itimer_value.tv_usec += 1000000;
		itp->itimer_value.tv_sec--;
	}
	itp->itimer_value.tv_usec -= usec;
	if (timerisset(&itp->itimer_value))
		return (1);
expire:
	if (itp->itimer_reload == 0)
		itp->itimer_value.tv_usec = 0;
	else
		itp->itimer_value = itp->itimer_interval;
	return (0);
}

#ifndef NOCOMPAT
otime()
{

	u.u_r.r_time = time.tv_sec;
}

#include "../h/timeb.h"

oftime()
{
	register struct a {
		struct	timeb	*tp;
	} *uap;
	struct timeb t;

	uap = (struct a *)u.u_ap;
	(void) spl7();
	t.time = time.tv_sec;
	t.millitm = time.tv_usec / 1000;
	(void) spl0();
	t.timezone = tz.tz_minuteswest;
	t.dstflag = tz.tz_dsttime;
	if (copyout((caddr_t)&t, (caddr_t)uap->tp, sizeof(t)) < 0)
		u.u_error = EFAULT;
}
