/*	kern_time.c	5.4	82/09/06	*/

#include "../h/param.h"
#include "../h/dir.h"		/* XXX */
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/reg.h"
#include "../h/inode.h"
#include "../h/proc.h"

/* 
 * Time of day and interval timer support.
 */

gettimeofday()
{
	register struct a {
		struct	timeval *tp;
		struct	timezone *tzp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;
	int s;

	s = spl7(); atv = time; splx(s);
	if (copyout((caddr_t)&atv, (caddr_t)uap->tp, sizeof (atv))) {
		u.u_error = EFAULT;
		return;
	}
	if (uap->tzp == 0)
		return;
	/* SHOULD HAVE PER-PROCESS TIMEZONE */
	if (copyout((caddr_t)&tz, uap->tzp, sizeof (tz))) {
		u.u_error = EFAULT;
		return;
	}
}

settimeofday()
{
	register struct a {
		struct	timeval *tv;
		struct	timezone *tzp;
	} *uap = (struct a *)u.u_ap;
	struct timeval atv;
	struct timezone atz;

	if (copyin((caddr_t)uap->tv, (caddr_t)&atv, sizeof (struct timeval))) {
		u.u_error = EFAULT;
		return;
	}
	setthetime(&atv);
	if (uap->tzp && suser()) {
		if (copyin((caddr_t)uap->tzp, (caddr_t)&atz, sizeof (atz))) {
			u.u_error = EFAULT;
			return;
		}
	}
}

setthetime(tv)
	struct timeval *tv;
{
	register int delta;
	int s;

	if (!suser())
		return;
	boottime.tv_sec += tv->tv_sec - time.tv_sec;
	s = spl7(); time = *tv; splx(s);
	clockset();
}

timevaladd(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec += t2->tv_sec;
	t1->tv_usec += t2->tv_usec;
	timevalfix(t1);
}

timevalsub(t1, t2)
	struct timeval *t1, *t2;
{

	t1->tv_sec -= t2->tv_sec;
	t1->tv_usec -= t2->tv_usec;
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
	if (copyout((caddr_t)itp, uap->itv, sizeof (struct itimerval)))
		u.u_error = EFAULT;
	splx(s);
}

setitimer()
{
	register struct a {
		u_int	which;
		struct	itimerval *itv, *oitv;
	} *uap = (struct a *)u.u_ap;
	struct itimerval aitv;
	int s;

	if (uap->which > 2) {
		u.u_error = EINVAL;
		return;
	}
	if (copyin((caddr_t)uap->itv, (caddr_t)&aitv,
	    sizeof (struct itimerval))) {
		u.u_error = EFAULT;
		return;
	}
	if (uap->oitv) {
		uap->itv = uap->oitv;
		getitimer();
	}
	if (itimerfix(&aitv.it_value) || itimerfix(&aitv.it_interval)) {
		u.u_error = EINVAL;
		return;
	}
	s = spl7();
	if (uap->which == ITIMER_REAL)
		u.u_procp->p_realtimer = aitv;
	else
		u.u_timer[uap->which] = aitv;
	splx(s);
}

itimerfix(tv)
	struct timeval *tv;
{

	if (tv->tv_sec < 0 || tv->tv_usec < 0)
		return (EINVAL);
	if (tv->tv_sec == 0 && tv->tv_usec < tick)
		tv->tv_usec = tick;
	return (0);
}

itimerdecr(itp, usec)
	register struct itimerval *itp;
	int usec;
{

	if (itp->it_value.tv_usec < usec) {
		if (itp->it_value.tv_sec == 0) {
			usec -= itp->it_value.tv_usec;
			goto expire;
		}
		itp->it_value.tv_usec += 1000000;
		itp->it_value.tv_sec--;
	}
	itp->it_value.tv_usec -= usec;
	usec = 0;
	if (timerisset(&itp->it_value))
		return (1);
expire:
	if (timerisset(&itp->it_interval)) {
		itp->it_value = itp->it_interval;
		itp->it_value.tv_usec -= usec;
		if (itp->it_value.tv_usec < 0) {
			itp->it_value.tv_usec += 1000000;
			itp->it_value.tv_sec--;
		}
	} else
		itp->it_value.tv_usec = 0;
	return (0);
}

#ifndef NOCOMPAT
otime()
{

	u.u_r.r_time = time.tv_sec;
}

ostime()
{
	register struct a {
		int	time;
	} *uap = (struct a *)u.u_ap;
	struct timeval tv;

	tv.tv_sec = uap->time;
	tv.tv_usec = 0;
	setthetime(&tv);
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
#endif
